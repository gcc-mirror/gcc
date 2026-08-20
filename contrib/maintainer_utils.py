#!/usr/bin/env python3

# Utilities for manipulating/testing the MAINTAINERS.yml data.

# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

import json
import jsonschema
import sys
import unidecode
import yaml

# Prefer the libYAML implementations if available.  Fall back to the
# pure python versions as a backup.
try:
    from yaml import CLoader as Loader, CDumper as Dumper
except:
    from yaml import Loader, Dumper

label_pattern = '[a-zA-Z][-+a-zA-Z0-9]*(/[a-zA-Z][-+a-zA-Z0-9]*)+'

maintainer_schema = {
    '$schema': 'https://json-schema.org/draft-04/schema',
    'type': 'object',
    'properties': {
        'users': {
            'type': 'array',
            'items': {
                'type': 'object',
                'properties': {
                    'sn': {
                        'type': 'string',
                        'minLength': 1,
                    },
                    'cn': {
                        'type': 'string',
                        'minLength': 1,
                    },
                    'email': {
                        'type': 'array',
                        'items': {
                            'type': 'string',
                            'format': 'email',
                        },
                        'minItems': 1,
                    },
                    'inactive_email': {
                        'type': 'array',
                        'items': {
                            'type': 'string',
                            'format': 'email',
                        },
                        'minItems': 1,
                    },
                    'DCO': {
                        'type': 'array',
                        'items': {
                            'type': 'string',
                            'format': 'email',
                        },
                        "minItems": 1,
                    },
                    'inactive_DCO': {
                        'type': 'array',
                        'items': {
                            'type': 'string',
                            'format': 'email',
                        },
                        "minItems": 1,
                    },
                    'roles': {
                        'type': 'array',
                        'items': {
                            'oneOf': [
                                {
                                    'type': 'string',
                                    'enum': ['Global', 'WriteAfter', 'BZ'],
                                },
                                {
                                    'type': 'object',
                                    'properties': {
                                        'WriteAfter': {
                                            'type': 'string',
                                            'format': 'email',
                                        },
                                    },
                                    'additionalProperties': False,
                                },
                                {
                                    'type': 'object',
                                    'properties': {
                                        'Maintainer': {
                                            'type': 'string',
                                        },
                                        'Reviewer': {
                                            'type': 'string',
                                        },
                                        'email': {
                                            'type': 'string',
                                            'format': 'email',
                                        },
                                    },
                                    'additionalProperties': False,
                                    'oneOf': [
                                        {'required': ["Maintainer"]},
                                        {'required': ["Reviewer"]},
                                    ],
                                },
                            ],
                        },
                        'minItems': 1,
                    },
                    'account': {
                        'type': 'string',
                        'minLength': 1,
                        'pattern': '[a-zA-Z][a-zA-Z0-9]*',
                    },
                    'forgeid': {
                        'type': 'string',
                        'minLength': 1,
                    },
                    'aliases': {
                        'type': 'array',
                        'items': {
                            'type': 'string',
                            'minLength': 1,
                        },
                        'minItems': 1,
                    },
                    'inactive': {
                        'type': 'boolean',
                    },
                },
                'additionalProperties': False,
                'required': ['sn', 'cn', 'email'],
                'anyOf': [
                    {'required': ['roles']},
                    {'required': ['DCO']},
                    {'required': ['inactive_DCO']},
                ],
            },
        },
        'subsystems': {
            'type': 'array',
            'items': {
                'type': 'object',
                'properties': {
                    'name': {
                        'type': 'string',
                        'minLength': 1,
                    },
                    'class': {
                        'type': 'string',
                        'minLength': 1,
                    },
                    'labels': {
                        'type': 'array',
                        # Disabled until we populate the labels
                        # 'minItems': 1,
                        'items': {
                            'type': 'string',
                            'minLength': 3,
                            'pattern': label_pattern,
                        },
                    },
                    'teams': {
                        'type': 'array',
                        'minItems': 1,
                        'items': {
                            'type': 'string',
                            'minLength': 3,
                        },
                    },
                },
                'additionalProperties': False,
                # Todo, add 'team' once the data is fully populated.
                'required': ['name', 'labels'],
            },
        },
    },
}

error_count = 0
warning_count = 0


def _fatal(msg):
    print(f"fatal: {msg}", file=sys.stderr)
    sys.exit(1)


def _error(msg):
    global error_count
    print(f"error: {msg}", file=sys.stderr)
    error_count += 1


def _warning(msg):
    global warning_count
    print(f"warning: {msg}", file=sys.stderr)
    warning_count += 1


def _format_path(path, data):
    """Try to turn a validation error path into something more readable"""
    elts = [""]
    cur = data
    try:
        for elt in path:
            if isinstance(cur, list) and isinstance(elt, int):
                item = cur[elt]
                if isinstance(item, dict) and 'cn' in item:
                    elts[-1] += f"[cn = {item['cn']}]"
                elif isinstance(item, dict) and 'name' in item:
                    elts[-1] += f"[name = {item['name']}]"
                else:
                    elts[-1] += f"[(anon index): {elt}]"
                cur = item
            else:
                cur = cur[elt]
                elts.append(str(elt))
    except Exception as e:
        print('/'.join(elts))
        raise e
    if elts[0] == "":
        elts = elts[1:]
    return '/'.join(elts)


def _check_schema(data):
    try:
        schema = jsonschema.validators.Draft4Validator(maintainer_schema)
        errors = sorted(schema.iter_errors(data), key=lambda e: e.path)
        for err in errors:
            _error(f"{_format_path(err.path, data)}: {err.message}")
    except jsonschema.exceptions.SchemaError as e:
        _fatal(str(e))
    return


def _unilower(txt):
    """return a lower-case version of txt, mapping accented characters
    onto their ASCII near equivalents."""
    return unidecode.unidecode(txt).lower()


def _check_dco(user):
    # An email addrss in a DCO entry must also be listed in either the
    # active emails list, or the inactive_emails list.
    emails = user['email']
    for dco in user.get('DCO', []):
        if dco not in emails:
            _error(f"User: {user['cn']} DCO {dco} not listed in emails")
    emails = user.get('inactive_email', [])
    for dco in user.get('inactive_DCO', []):
        if dco not in emails:
            _error(
                f"User: {user['cn']} inactive_DCO {dco} not listed in inactive_emails"
            )
        if dco in user.get('DCO', []):
            _error(f"User: {user['cn']} inactive_DCO {dco} also listed in DCO")


def sort_users(user_data):
    """Sort the user data into canonical order."""
    return sorted(
        user_data,
        key = lambda k: (
            _unilower(k['sn']),
            _unilower(k['cn']),
            k.get('account', ''),
            k.get('forgeid', ''),
        )
    )


def validate(data):
    """Check the data against the schema and our own consistency checks"""
    _check_schema(data)
    # The data conforms to the expected schema, so now run some data
    # validation checks.

    # Maintainer and Reviewer roles need to match entries in the
    # subsystems list; Maintainer entires must also have a class entry, though
    # that is optional for Reviewers.
    for u in data['users']:
        if 'DCO' in u or 'inactive_DCO' in u:
            _check_dco(u)
        # The schema ensures that at least one of 'DCO',
        # 'inactive_DCO' or 'roles' exists, so if roles is missing,
        # we're done.
        if 'roles' not in u:
            continue
        # Users with the 'BZ' role should not have any other roles; we
        # can quickly skip the additional checks if that is the case.
        if len(u['roles']) == 1 and u['roles'][0] == 'BZ':
            continue
        seen_writeafter = False
        for r in u['roles']:
            if isinstance(r, str):
                if r == 'BZ':
                    _error(
                        f"User '{u['cn']}' has 'BZ' role as well as others."
                    )
                if r == 'WriteAfter':
                    seen_writeafter = True
                continue
            if 'WriteAfter' in r:
                seen_writeafter = True
            need_class = True
            n = r.get('Maintainer')
            if not n:
                need_class = False
                n = r.get('Reviewer')
            if n:
                subsystems = list(
                    filter(lambda s: s['name'] == n, data['subsystems'])
                )
                if len(subsystems) == 1:
                    if need_class and not 'class' in subsystems[0]:
                        _error(f"subsystem '{n}' missing a class.")
                elif len(subsystems) == 0:
                    _error(f"No subsystem entry for '{n}'.")
                else:
                    _error(f"Multiple subsystem entries for '{n}'.")
        if not seen_writeafter:
            _error(f"User '{u['cn']}' lacks WriteAfter role.")

    sorted = sort_users(data['users'])
    if sorted != data['users']:
        _error("User data is incorrectly sorted.")
        for right, wrong in zip(sorted, data['users']):
            if right != wrong:
                print(
                    f"First incorrect entry is for {wrong['cn']}",
                    file=sys.stderr
                )
                break
    if error_count:
        sys.exit(1)
    return


def load(file):
    with open(file, "r", encoding="utf-8") as f:
        data = yaml.load(f, Loader=Loader)
    return data


def store(data, file=None, fd=sys.stdout):
    data['users'] = sort_users(data['users'])
    # Make sure we don't write something that is not conformant
    validate(data)
    if file:
        fd = open(file, "w", encoding="utf-8")
    print("# If you edit this file, please validate with:",
          file=fd)
    print("#    contrib/maintainer_utils.py <MAINTAINERS.yml>",
          file=fd)
    print("# before committing.\n", file=fd)
    yaml.dump(data, fd, allow_unicode=True, sort_keys=False)
    if file:
        fd.close()

def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} path-to-MAINTAINERS.yml")
        return 1
    validate(load(sys.argv[1]))
    return 0


if __name__ == "__main__":
    sys.exit(main())
