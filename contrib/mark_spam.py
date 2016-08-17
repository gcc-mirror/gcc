#!/usr/bin/env python3
#
# Script to mark bunch of PRs as spam 
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
# <http://www.gnu.org/licenses/>.  */
#
#
#

import requests
import json
import argparse

base_url = 'https://gcc.gnu.org/bugzilla/rest.cgi/'

def mark_as_spam(id, api_key, verbose):
    print('Marking as spam: PR%d' % id)
    # 1) get bug info to find 'cc'
    u = base_url + 'bug/' + str(id)
    r = requests.get(u)
    response = json.loads(r.text)

    if 'error' in response and response['error']:
        print(response['message'])
        return

    # 2) mark the bug as spam
    bug = response['bugs'][0]
    creator = bug['creator']
    cc_list = bug['cc']
    data = {
        'status': 'RESOLVED',
        'resolution': 'INVALID',
        'summary': 'spam',
        'ids': [id],
        'api_key': api_key,
        'comment': { 'comment': 'spam'},
        'product': 'gcc',
        'component': 'spam',
        'version': 'unknown',
        'cc': {'remove': cc_list},
        'priority': 'P5',
        'severity': 'trivial',
        'url': '',
        'assigned_to': 'unassigned@gcc.gnu.org' }

    r = requests.put(u, json = data)
    if verbose:
        print(r)
        print(r.text)

    # 3) mark the first comment as spam
    r = requests.get(u + '/comment')
    response = json.loads(r.text)
    for c in response['bugs'][str(id)]['comments']:
        if c['creator'] == creator:
            comment_id = c['id']
            u2 = '%sbug/comment/%d/tags' % (base_url, comment_id)
            print(u2)
            r = requests.put(u2, json = {'comment_id': comment_id, 'add': ['spam'], 'api_key': api_key})
            if verbose:
                print(r)
                print(r.text)

    # 4) mark all attachments as spam
    r = requests.get(u + '/attachment')
    response = json.loads(r.text)
    attachments = response['bugs'][str(id)]
    for a in attachments:
        attachment_id = a['id']
        url = '%sbug/attachment/%d' % (base_url, attachment_id)
        r = requests.put(url, json = {'ids': [attachment_id],
            'summary': 'spam',
            'file_name': 'spam',
            'content_type': 'application/x-spam',
            'is_obsolete': True,
            'api_key': api_key})
        if verbose:
            print(r)
            print(r.text)

parser = argparse.ArgumentParser(description='Mark Bugzilla issues as spam.')
parser.add_argument('api_key', help = 'API key')
parser.add_argument('range', help = 'Range of IDs, e.g. 10-23,24,25,27')
parser.add_argument('--verbose', action = 'store_true', help = 'Verbose logging')

args = parser.parse_args()

chunks = args.range.split(',')
for c in chunks:
    parts = list(map(lambda x: int(x), c.split('-')))
    if len(parts) == 1:
        r = [parts[0]]
    else:
        r = range(parts[0], parts[1] + 1)

    for id in r:
        mark_as_spam(id, args.api_key, args.verbose)
