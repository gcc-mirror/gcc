#!/usr/bin/env python3

# Manage maintainer teams on the forge

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

# This script is used to syncronize the data in the MAINTAINERS.yml
# file with the teams used by the forge for notifications.

# The script is set up with all defaults for the sourceware forge
# except for an API token.  The token's minimum permissions are
#   write:organization
# but only owners of the 'gcc' organization will be able to generate
# useable write tokens.
# You can enter the API token either via the environment variable
# APIKEY, or when requested.


import getpass
import json
import math
import os
import pprint
import sys

import urllib.parse
import urllib.request
import http.client

from optparse import OptionParser

import maintainer_utils as maintutils

defaults = {
    "PROJECT": "gcc",
    "REPO": "gcc",
    "FORGE": "https://forge.sourceware.org/api/v1",
}

# These are teams in the forge that this script will not attempt to manage
# because the relevant data is not part of MAINTAINERS.yml
unmanaged_teams = [
    'Automation',
    'Collaborators',
    'Maintainers.Global',   # Remove once fully implemented
    'Owners',
    'Release.Managers',
    'Reviewers',            # Remove once fully implemented
    'Write.After.Approval', # Remove once fully implemented
]

opts = None
error_count = 0


def verbose(msg) -> None:
    if opts.verbose:
        print(msg)
    return


class Forge:
    def __init__(self):
        self._setup()

    def _setup(self) -> None:
        self.project = os.getenv("PROJECT", defaults['PROJECT'])
        self.repo = os.getenv("REPO", defaults['REPO'])
        self.forge = os.getenv("FORGE", defaults['FORGE'])
        self.apikey = os.getenv("APIKEY") or getpass.getpass(
            prompt="API key: "
        )
        verbose(f"Accessing {self.forge}/ for org {self.project}.")
        return

    def _send_request(
        self,
        url,
        method='GET',
    ) -> http.client.HTTPResponse:
        # For safety while developing.
        if method == 'GET' or not opts.dry_run:
            headers = {
                'Authorization': f"token {self.apikey}",
                'accept': 'application/json',
                'Content-Type': 'application/json',
            }
            request = urllib.request.Request(
                url,
                headers=headers,
                method=method,
            )
            return urllib.request.urlopen(request)
        return None

    # The rest API does not guarantee to return all the data with a
    # single request.  We support fetching by pages of up to limit.
    # Note that the server may return less than this if increased too
    # far. 50 seems a reasonable size; we don't expect there to be
    # more than a couple of pages at that size.
    def _get_teams_page(self, page=1, limit=50) -> tuple[
        list,
        int,
    ]:
        fetch = urllib.parse.urlencode(
            {
                "page": page,
                "limit": limit,
            }
        )
        url = f"{self.forge}/orgs/{self.project}/teams?{fetch}"
        verbose(f"Requesting {url}")
        reply = self._send_request(url)
        item_count = int(reply.headers.get("X-Total-Count", -1))
        pages = math.ceil(item_count / limit) if item_count > 0 else 1
        return json.load(reply), pages

    def _get_team_members_page(self, id, page=1, limit=50) -> tuple[
        list,
        int,
    ]:
        fetch = urllib.parse.urlencode(
            {
                "page": page,
                "limit": limit,
            }
        )
        url = f"{self.forge}/teams/{id}/members?{fetch}"
        verbose(f"GET {url}")
        reply = self._send_request(url)
        item_count = int(reply.headers.get("X-Total-Count", -1))
        pages = math.ceil(item_count / limit) if item_count > 0 else 1
        return json.load(reply), pages

    def fetch_teams(self) -> list:
        teams, pages = self._get_teams_page()
        if pages == 1:
            return teams
        for page in range(2, pages + 1):
            teams_page, _ = self._get_teams_page(page=page)
            teams.extend(teams_page)
        return teams

    def fetch_team_members(self, id) -> list:
        members, pages = self._get_team_members_page(id)
        if pages == 1:
            return members
        for page in range(2, pages + 1):
            members_page, _ = self._get_team_members_page(id, page=page)
            members.extend(members_page)
        return members

    def manage_team_member(self, id, login, remove=False) -> None:
        url = f"{self.forge}/teams/{id}/members/{login}"
        action = 'DELETE' if remove else 'PUT'
        verbose(f"{action} {url}")
        reply = self._send_request(url, method=action)


class DesiredTeam:
    def __init__(
        self,
        teamname,
        role_filter,
        user_data,
        subsystem=None,
    ) -> None:
        self.name = teamname
        self.members = []
        self.accounts = set()
        self.role_filter = role_filter
        self.add_users(subsystem, user_data)

    @staticmethod
    def subsystem_filter(subsystem_name, roles) -> bool:
        return any(
            isinstance(role, dict)
            and (
                role.get('Maintainer') == subsystem_name
                or role.get('Reviewer') == subsystem_name
            )
            for role in roles
        )

    @staticmethod
    def maintainers_filter(subsystem_name, roles) -> bool:
        return any(
            role == 'Global'
            or (isinstance(role, dict) and 'Maintainer' in role)
            for role in roles
        )

    @staticmethod
    def global_reviewers_filter(subsystem_name, roles) -> bool:
        return any(
            role == 'Global'
            for role in roles
        )

    @staticmethod
    def reviewers_filter(subsystem_name, roles) -> bool:
        return any(
            isinstance(role, dict) and 'Reviewer' in role
            for role in roles
        )

    @staticmethod
    def write_after_filter(subsystem_name, roles) -> bool:
        return any(
            role == "WriteAfter"
            for role in roles
        )

    def add_users(self, subsystem_name, user_data) -> None:
        new_members = [
            u
            for u in user_data
            if (forgeid := u.get("forgeid")) is not None
            and forgeid not in self.accounts
            and self.role_filter(subsystem_name, u['roles'])
        ]
        self.members.extend(new_members)
        self.accounts.update(u["forgeid"] for u in new_members)


class DesiredTeamList:
    def __init__(self, data) -> None:
        # Start with the static teams that are not based on specific
        # subsytems.
        self.teams = {
            'Maintainers.Global': DesiredTeam(
                "Maintainers.Global",
                DesiredTeam.global_reviewers_filter,
                data['users'],
            ),
            'Maintainers': DesiredTeam(
                "Maintainers", DesiredTeam.maintainers_filter, data['users']
            ),
            'Reviewers': DesiredTeam(
                "Reviewers", DesiredTeam.reviewers_filter, data['users']
            ),
            'Write.After.Approval': DesiredTeam(
                "Write.After.Approval",
                DesiredTeam.write_after_filter,
                data['users'],
            ),
        }
        # Now add teams for subsystems where a forge team has been
        # set up.  This is a many<->many mapping: some subsystems
        # will impact multiple teams and some teams my cover multiple
        # subsystems and there may be overlaps in team membership.
        for subsystem in filter(
                lambda s: 'teams' in s,
                data['subsystems']
        ):
            for t in subsystem['teams']:
                if t in self.teams:
                    self.teams[t].add_users(
                        subsystem['name'],
                        data['users'],
                    )
                else:
                    self.teams[t] = DesiredTeam(
                        t,
                        DesiredTeam.subsystem_filter,
                        data['users'],
                        subsystem=subsystem['name'],
                    )

    def get_by_name(self, name) -> DesiredTeam:
        return self.teams.get(name)


class ExistingTeam:
    def __init__(self, forge, team) -> None:
        self._forge = forge
        self._team = team
        self._members = {
            m['login']: m
            for m in forge.fetch_team_members(team['id'])
        }

    def get_member_names(self) -> list:
        return self._members.keys()

    def has_member(self, name) -> bool:
        return self._members.get(name) != None

    def add_member(self, name) -> None:
        self._forge.manage_team_member(self._team['id'], name)

    def remove_member(self, name) -> None:
        self._forge.manage_team_member(self._team['id'], name, remove=True)


class ExistingMaintainers:
    def __init__(self, forge):
        self._forge = forge
        self._teams = {
            t['name']: ExistingTeam(forge, t)
            for t in forge.fetch_teams()
            if not t['name'] in unmanaged_teams
        }

    def get_by_name(self, name) -> ExistingTeam:
        return self._teams.get(name)

    def get_team_names(self) -> list:
        return self._teams.keys()


def update_teams(forge, needed_teams, existing_teams) -> None:
    global error_count
    for name, team in needed_teams.teams.items():
        # Skip any teams we don't manage
        if name in unmanaged_teams:
            continue
        existing = existing_teams.get_by_name(name)
        if existing:
            verbose(f"Checking membership of {name}")
            # First add any missing members; while doing this, build up
            # a set of forgeids in the team so that we can quickly search
            # the team list for pass 2
            member_names = set()
            for member in team.members:
                member_names.add(member['forgeid'])
                if not existing.has_member(member['forgeid']):
                    # Add new member
                    verbose(
                        f"  missing: {member['cn']} ({member['forgeid']})"
                    )
                    existing.add_member(member['forgeid'])
                else:
                    verbose(f"  OK: {member['cn']} ({member['forgeid']})")
            # Secondly, remove any excess members
            for member in existing.get_member_names():
                if not member in member_names:
                    verbose(f"  remove: {member}")
                    existing.remove_member(member)
        else:
            error_count += 1
            print(f"Team '{name}' does not exist on the forge")
    # Scan the existing teams and report any that do not exist in
    # either MAINTAINERS.yml or in the ignore list.
    for existing in existing_teams.get_team_names():
        if (
            not needed_teams.get_by_name(existing)
            and existing not in unmanaged_teams
        ):
            error_count += 1
            if len(existing_teams.get_by_name(existing).get_member_names()):
                print(f"Unexpected team '{existing}' found on forge")
            else:
                print(
                    f"Forge team '{existing}' has no members and can be removed"
                )


def main() -> int:
    global opts
    optp = OptionParser("Usage: %prog [<options>] <maintainers.yml>")
    optp.add_option(
        "-v", "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
    )
    optp.add_option(
        "-n", "--dry-run",
        action="store_true",
        dest="dry_run",
        default=False,
    )
    opts, args = optp.parse_args()
    if len(args) != 1:
        optp.print_help()
        return 1

    maint_data = maintutils.load(args[0])
    maintutils.validate(maint_data)
    needed_teams = DesiredTeamList(maint_data)
    forge = Forge()
    existing_teams = ExistingMaintainers(forge)
    update_teams(forge, needed_teams, existing_teams)
    if opts.verbose:
        print("Existing managed teams and members:")
        for team in existing_teams.get_team_names():
            print(team)
            for m in existing_teams.get_by_name(team).get_member_names():
                print(f"  {m}")
        print("\nDesired teams and members:")
        for name, team in needed_teams.teams.items():
            unmanaged = " (unmanaged)" if name in unmanaged_teams else ""
            print(f"{name}{unmanaged}")
            for m in team.members:
                print(f"  {m['forgeid']}")
    return 0 if not error_count else 1


if __name__ == "__main__":
    sys.exit(main())
