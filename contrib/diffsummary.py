#!/usr/bin/env python3
# Show or diff test_summary output to latest posted test result for a platform
# test_summary -t | diffsummary.py (compare test result from current build to last posted)
# diffsummary.py --show     (only show last posted)
from urllib.request import urlopen
from html.parser import HTMLParser
import time
import re
import argparse
import platform
import tempfile
import os
import sys

baseurl = "https://gcc.gnu.org/pipermail/gcc-testresults/"

ap = argparse.ArgumentParser(description="Diff stdin to latest posted test result and set exit code to result")
ap.add_argument("--branch", default="experimental",
                help="Branch to match (regex substring match)")
ap.add_argument("--arch", default=platform.machine() + ".*" + platform.system().lower(),
                help="architecture to match (regex substring match)")
ap.add_argument("--retrytime", default=1, type=int,
                help="time to wait before fetching next page (fractional seconds)")
ap.add_argument("--show", help="Show last test result, but do not diff", action="store_true", default=False)
ap.add_argument("--url", help="Show URLs downloaded", action="store_true")
ap.add_argument("--diff", help="Diff program to use (default diff)", default="diff")
ap.add_argument("--diff-args", help="Diff arguments to use (default -u)", default="-u")
ap.add_argument("--skip", type=int, default=0, help="Skip first N posted test results")
args = ap.parse_args()

class ParseMonths(HTMLParser):
    def __init__(self):
        super().__init__()
        self.months = []
    def handle_starttag(self, tag, attrs):
        if tag == "a" and "thread.html" in attrs[0][1]:
            self.months.append(attrs[0][1])

class ParseThread(HTMLParser):
    def __init__(self):
        super().__init__()
        self.link = None
        self.target = []
    def handle_starttag(self, tag, attrs):
        if (tag == "a"
            and attrs[0][1][0].isdigit()
            and attrs[0][1].endswith(".html")):
            self.link = attrs[0][1]
        else:
            self.link = None
    def handle_data(self, data):
        if (self.link
            and re.search(args.branch, data)
            and re.search(args.arch, data)):
            self.target.append(thread.link)

class ParseArticle(HTMLParser):
    def __init__(self):
        super().__init__()
        self.contents = None
        self.pre = False
        self.data = None
    def handle_starttag(self, tag, attrs):
        self.pre = tag == "pre"
    def handle_data(self, data):
        if self.pre and not self.data:
            self.data = data

def my_urlopen(url):
    if args.url:
        print(url)
    return urlopen(url)

with my_urlopen(baseurl) as index:
    months = ParseMonths()
    months.feed(index.read().decode('utf-8'))
if len(months.months) == 0:
    sys.exit("no months found")
for month in months.months:
    with my_urlopen(baseurl + month) as m:
        thread = ParseThread()
        thread.feed(m.read().decode('utf-8'))
        if thread.target:
            if args.skip > len(thread.target):
                args.skip -= len(thread.target)
                continue
            url = (baseurl + month).replace("thread.html", thread.target[-1 - args.skip])
            with my_urlopen(url) as t:
                article = ParseArticle()
                article.feed(t.read().decode('utf-8'))
                if args.show:
                    print(article.data)
                    break
                with tempfile.NamedTemporaryFile(delete=False) as tf:
                    tf.write(article.data.encode('utf-8'))
                    tf.close()
                    cmd = f"{args.diff} {args.diff_args} - {tf.name}"
                    print(cmd)
                    ret = os.system(cmd)
                    os.unlink(tf.name)
                    sys.exit(ret)
                break
        time.sleep(args.retrytime)
