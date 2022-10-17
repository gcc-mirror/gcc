#!/usr/bin/env python3
#
# Check gcc.pot file for stylistic issues as described in
# https://gcc.gnu.org/onlinedocs/gccint/Guidelines-for-Diagnostics.html,
# especially in gcc-internal-format messages.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

import argparse
import re
from collections import Counter
from typing import Dict, Match

import polib

seen_warnings = Counter()


def location(msg: polib.POEntry):
    if msg.occurrences:
        occ = msg.occurrences[0]
        return f'{occ[0]}:{occ[1]}'
    return '<unknown location>'


def warn(msg: polib.POEntry,
         diagnostic_id: str, diagnostic: str, include_msgid=True):
    """
    To suppress a warning for a particular message,
    add a line "#, gcclint:ignore:{diagnostic_id}" to the message.
    """

    if f'gcclint:ignore:{diagnostic_id}' in msg.flags:
        return

    seen_warnings[diagnostic] += 1

    if include_msgid:
        print(f'{location(msg)}: {diagnostic} in {repr(msg.msgid)}')
    else:
        print(f'{location(msg)}: {diagnostic}')


def lint_gcc_internal_format(msg: polib.POEntry):
    """
    Checks a single message that has the gcc-internal-format. These
    messages use a variety of placeholders like %qs, %<quotes%> and
    %q#E.
    """

    msgid: str = msg.msgid

    def outside_quotes(m: Match[str]):
        before = msgid[:m.start(0)]
        return before.count('%<') == before.count('%>')

    def lint_matching_placeholders():
        """
        Warns when literal values in placeholders are not exactly equal
        in the translation. This can happen when doing copy-and-paste
        translations of similar messages.

        To avoid these mismatches in the first place,
        structurally equal messages are found by
        lint_diagnostics_differing_only_in_placeholders.

        This check only applies when checking a finished translation
        such as de.po, not gcc.pot.
        """

        if not msg.translated():
            return

        in_msgid = re.findall('%<[^%]+%>', msgid)
        in_msgstr = re.findall('%<[^%]+%>', msg.msgstr)

        if set(in_msgid) != set(in_msgstr):
            warn(msg,
                 'placeholder-mismatch',
                 f'placeholder mismatch: msgid has {in_msgid}, '
                 f'msgstr has {in_msgstr}',
                 include_msgid=False)

    def lint_option_outside_quotes():
        for match in re.finditer(r'\S+', msgid):
            part = match.group()
            if not outside_quotes(match):
                continue

            if part.startswith('-'):
                if len(part) >= 2 and part[1].isalpha():
                    if part == '-INF':
                        continue

                    warn(msg,
                         'option-outside-quotes',
                         'command line option outside %<quotes%>')

            if part.startswith('__builtin_'):
                warn(msg,
                     'builtin-outside-quotes',
                     'builtin function outside %<quotes%>')

    def lint_plain_apostrophe():
        for match in re.finditer("[^%]'", msgid):
            if outside_quotes(match):
                warn(msg, 'apostrophe', 'apostrophe without leading %')

    def lint_space_before_quote():
        """
        A space before %< is often the result of string literals that
        are joined by the C compiler and neither literal has a space
        to separate the words.
        """

        for match in re.finditer('(.?[a-zA-Z0-9])%<', msgid):
            if match.group(1) != '%s':
                warn(msg,
                     'no-space-before-quote',
                     '%< directly following a letter or digit')

    def lint_underscore_outside_quotes():
        """
        An underscore outside of quotes is used in several contexts,
        and many of them violate the GCC Guidelines for Diagnostics:

        * names of GCC-internal compiler functions
        * names of GCC-internal data structures
        * static_cast and the like (which are legitimate)
        """

        for match in re.finditer('_', msgid):
            if outside_quotes(match):
                warn(msg,
                     'underscore-outside-quotes',
                     'underscore outside of %<quotes%>')
                return

    def lint_may_not():
        """
        The term "may not" may either mean "it could be the case"
        or "should not". These two different meanings are sometimes
        hard to tell apart.
        """

        if re.search(r'\bmay not\b', msgid):
            warn(msg,
                 'ambiguous-may-not',
                 'the term "may not" is ambiguous')

    def lint_unbalanced_quotes():
        if msgid.count('%<') != msgid.count('%>'):
            warn(msg,
                 'unbalanced-quotes',
                 'unbalanced %< and %> quotes')

        if msg.translated():
            if msg.msgstr.count('%<') != msg.msgstr.count('%>'):
                warn(msg,
                     'unbalanced-quotes',
                     'unbalanced %< and %> quotes')

    def lint_single_space_after_sentence():
        """
        After a sentence there should be two spaces.
        """

        if re.search(r'[.] [A-Z]', msgid):
            warn(msg,
                 'single-space-after-sentence',
                 'single space after sentence')

    def lint_non_canonical_quotes():
        """
        Catches %<%s%>, which can be written in the shorter form %qs.
        """
        match = re.search("%<%s%>|'%s'|\"%s\"|`%s'", msgid)
        if match:
            warn(msg,
                 'non-canonical-quotes',
                 f'placeholder {match.group()} should be written as %qs')

    lint_option_outside_quotes()
    lint_plain_apostrophe()
    lint_space_before_quote()
    lint_underscore_outside_quotes()
    lint_may_not()
    lint_unbalanced_quotes()
    lint_matching_placeholders()
    lint_single_space_after_sentence()
    lint_non_canonical_quotes()


def lint_diagnostics_differing_only_in_placeholders(po: polib.POFile):
    """
    Detects messages that are structurally the same, except that they
    use different plain strings inside %<quotes%>. These messages can
    be merged in order to prevent copy-and-paste mistakes by the
    translators.

    See bug 90119.
    """

    seen: Dict[str, polib.POEntry] = {}

    for msg in po:
        msg: polib.POEntry
        msgid = msg.msgid

        normalized = re.sub('%<[^%]+%>', '%qs', msgid)
        if normalized not in seen:
            seen[normalized] = msg
            seen[msgid] = msg
            continue

        prev = seen[normalized]
        warn(msg,
             'same-pattern',
             f'same pattern for {repr(msgid)} and '
             f'{repr(prev.msgid)} in {location(prev)}',
             include_msgid=False)


def lint_file(po: polib.POFile):
    for msg in po:
        msg: polib.POEntry

        if not msg.obsolete and not msg.fuzzy:
            if 'gcc-internal-format' in msg.flags:
                lint_gcc_internal_format(msg)

    lint_diagnostics_differing_only_in_placeholders(po)


def main():
    parser = argparse.ArgumentParser(description='')
    parser.add_argument('file', help='pot file')

    args = parser.parse_args()

    po = polib.pofile(args.file)
    lint_file(po)

    print()
    print('summary:')
    for entry in seen_warnings.most_common():
        if entry[1] > 1:
            print(f'{entry[1]}\t{entry[0]}')


if __name__ == '__main__':
    main()
