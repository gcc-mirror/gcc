#! /usr/bin/env python3

# Add a new Write-After account to MAINTAINERS.yml

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

import os
import pwd
import re
import sys
import unidecode
import yaml

from optparse import OptionParser

import maintainer_utils as maintutils

def unilower(txt):
    """return a lower-case version of txt, mapping accented characters
    onto their ASCII near equivalents."""
    return unidecode.unidecode(txt).lower()

def get_surname(name):
    parts = name.split()
    surname = parts[-1]

    if surname.startswith("d'"):
        surname = surname[2:]

    return surname

def ask(prompt, default, required=True):
    while True:
        print(f"{prompt} [{default if default else '(no default)'}]: ", end="",
              flush=True)
        result = sys.stdin.readline().strip()
        if result != "":
            return result
        if default or not required:
            return default

def email_valid(e):
    template = r"^[A-Za-z0-9._%+-]+@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,}$"
    if re.match(template, e):
        return True
    return False

def getuserdata():
    user = pwd.getpwuid(os.getuid())
    # Make an initial guess that the name and GCC account will match
    # the local account name.
    # GECOS fields sometime contain a comma-separated list; in that case
    # we only want the first component (the user's name)
    print ("Please enter the details of your Sourceware account.")
    print ("Some details have been guessed, based on your local account,")
    print ("but you can adjust as required.")
    newuser = {
        'sn': "",
        'cn': "",
        'email': [],
        'roles': [],
        'account': "",
    }
    newuser['cn'] = ask("Your name", user.pw_gecos.split(",", 1)[0])
    newuser['sn'] = ask("Your surname (only used for sorting entries)",
                        get_surname(newuser['cn']))
    account = ask("Your sourceware account", user.pw_name)
    forgeid = ask("Your sourceware forge ID (if you have one)", None,
                  required=False)
    while True:
        e = ask("Primary email address", None)
        if email_valid(e):
            break
        print ("That address does not look valid.")
    newuser['email'].append(e)
    while (e := ask("Additional email (return to stop)", None,
                    required=False)):
        if email_valid(e):
            newuser['email'].append(e)
        else:
            print ("That address does not look valid.  Ignored.")
    gcc_email = account + "@gcc.gnu.org"
    if not gcc_email in newuser['email']:
        print(f"Appending {gcc_email} to the list of email addresses")
        newuser['email'].append(gcc_email)
    newuser['roles'] = list()
    newuser['roles'].append('WriteAfter')
    newuser['account'] = account
    if forgeid:
        newuser['forgeid'] = forgeid
    print("If you are using a Developer Certificate of Origin (DCO)")
    print("you can add appropriate email addresses here")
    while (e := ask("DCO email (return to stop)", None, required=False)):
        if email_valid(e):
            newuser['roles'].append({'DCO': e})
        else:
            print ("That address does not look valid.  Ignored.")
    return newuser

def main():
    optp = OptionParser(usage="Usage: %prog [<options>] <maintainers.yml>")
    optp.add_option("-o", "--output", metavar="FILE",
                    default=None, dest="outfilename",
                    help="Write to FILE instead of updating original file")
    opts, args = optp.parse_args()
    if len(args) != 1:
        optp.print_help()
        sys.exit(1)

    data = maintutils.load(args[0])
    # Check the data before we try to update it.
    maintutils.validate(data)
    newuser = getuserdata()
    if not newuser:
        return 0
    existing = [u for u in filter(lambda x: (x.get('account')
                                             == newuser['account']),
                                  data['users'])]
    if len(existing):
        print(f"Sorry that account name ({newuser['account']}) has already been used.")
        print("Note, this script can only be used to add new accounts.")
        return 1
    data['users'].append (newuser)
    data['users'] = sorted(data['users'],
                           key = lambda k: (unilower(k['sn']),
                                            unilower(k['cn'])))
    if opts.outfilename and opts.outfilename != '-':
        outfd = open (opts.outfilename, "w", encoding="utf-8")
    elif opts.outfilename and opts.outfilename == '-':
        outfd = sys.stdout
    else:
        outfd = open (args[0], "w", encoding="utf-8")
    yaml.dump (data, outfd, allow_unicode = True, sort_keys = False)
    return 0

if __name__ == "__main__":
    sys.exit (main())
