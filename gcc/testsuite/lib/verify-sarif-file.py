# Verify that ARGV[1] is UTF-8 encoded and parseable as JSON
# For use by the verify-sarif-file directive

import json
import sys

sys.tracebacklimit = 0

fname = sys.argv[1]
with open(fname, encoding='utf-8') as f:
    json.load(f)
