#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np

import matplotlib
import subprocess
import sys
import re
import os


try:
    number_of_weeks = int(sys.argv[1])
except Exception as e:
    print("script requires an integer argument for the number of weeks")
    sys.exit(-1)


cwd_is_root_repo = os.path.isdir('.git')
if not cwd_is_root_repo:
    print("script must be ran from the root of the repo")
    sys.exit(-1)
    

change_list = []
for i in range(number_of_weeks):
    since = "%i weeks ago" % (i + 1)
    until = "%i weeks ago" % i if i > 0 else None

    command = [ "git", "whatchanged", "--since=%s" % since ]
    if until is not None:
        command.append("--until=%s" % until)

    specific_paths = [ 'gcc/rust', 'gcc/testsuite/rust', 'gcc/testsuite/rust.test' ]
    command += ['--'] + specific_paths
    
    result = subprocess.run(
        command, capture_output=True, text=True
    )
    raw_diff = result.stdout
    
    commit_lines = re.findall('commit [a-z0-9]{40}', raw_diff)
    commit_shas = list(map(lambda i: i.split(' ')[1], commit_lines))

    changes = {
        'files_changed': 0,
        'insertions': 0,
        'deletions': 0,
        'contributors': set(),
        'contributions': list()
    }

    short_stat_out = None
    if len(commit_shas) > 0:
        if len(commit_shas) == 1:
            short_stat_command = [ 'git', 'show', '%s' % commit_shas[0], '--shortstat' ]
            result = subprocess.run(
                short_stat_command, capture_output=True, text=True
            )
            raw_short_stat = result.stdout
            short_stat_out = raw_short_stat.split('\n')[-2].strip()
        
        else:
            from_sha = commit_shas[-1]
            to_sha = commit_shas[0]
            short_stat_command = [ 'git', 'diff', '%s..%s' % (from_sha, to_sha), '--shortstat' ]
            result = subprocess.run(
                short_stat_command, capture_output=True, text=True
            )
            raw_short_stat = result.stdout
            short_stat_out = raw_short_stat.strip()

    
    if short_stat_out is not None:
        # pull out the numbers via regex
        search = re.findall('[0-9]* file', short_stat_out)
        if search is not None:
            changes['files_changed'] = int(search[0].split(' ')[0])

        search = re.findall('[0-9]* insertion', short_stat_out)
        if search is not None:
            changes['insertions'] = int(search[0].split(' ')[0])

        search = re.findall('[0-9]* deletion', short_stat_out)
        if search is not None:
            if len(search) > 0:
                changes['deletions'] = int(search[0].split(' ')[0])

    
    # figure out the unique contributors each week
    for sha in commit_shas:
        show_command = [ "git", "show", "-s", sha ]
        result = subprocess.run(
            show_command, capture_output=True, text=True
        )
        author_str = re.findall('Author: [a-zA-Z0-9-è ]* <[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+>', result.stdout)
        author_tokens = author_str[0].split(' ')
        author = " ".join(author_tokens[1:])
        changes['contributors'].add(author)
        changes['contributions'].append(author)
        

    # lets see it then
    print(changes)

    # hack to get rid of the gcc-merge
    if changes['files_changed'] == 23432:
        change_list.append(change_list[-1])
    else:
        change_list.append(changes)


unique_contributors = set()
for i in change_list:
    for y in i['contributors']:
        unique_contributors.add(y)

# calculate average lines added and removed per week
total_ins_lines = sum(map(lambda i: i['insertions'], change_list))
avg_ins_lines_per_week = total_ins_lines / float(len(change_list))

total_del_lines = sum(map(lambda i: i['deletions'], change_list))
avg_del_lines_per_week = total_del_lines / float(len(change_list))

print("average lines added per week:", avg_ins_lines_per_week)
print("average lines deleted per week:", avg_del_lines_per_week)

# find number of contributions per unique_contributor
num_contribs = {}
for i in unique_contributors:
    num_contribs[i] = 0
    for change in change_list:
        for y in change['contributions']:
            if y == i:
                num_contribs[i] = num_contribs[i] + 1


contribs = []
for i in num_contribs:
    contribs.append((i, num_contribs[i]))

contribs.sort(key=lambda i: i[1], reverse=True)
for i in contribs:
    print(i[0], i[1])
    
                
# graph the change_list
weeks = list(map(lambda i: i, range(number_of_weeks)))
insertions = list(map(lambda i: i['insertions'], change_list))
deletions = list(map(lambda i: i['deletions'], change_list))
files_changed = list(map(lambda i: i['files_changed'], change_list))
num_contributors = list(map(lambda i: len(i['contributors']), change_list))

weeks.reverse()
insertions.reverse()
deletions.reverse()
files_changed.reverse()
num_contributors.reverse()

fix, axs = plt.subplots(2)

axs[0].set_title("number of lines added + removed")
axs[0].plot(weeks, insertions)
axs[0].plot(weeks, deletions)
# plt.plot(weeks, files_changed)

axs[1].set_title("number of contributors")
axs[1].plot(weeks, num_contributors)

plt.show()
