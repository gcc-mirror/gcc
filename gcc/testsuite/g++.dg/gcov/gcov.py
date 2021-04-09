import gzip
import json
import os


def gcov_from_env():
    # return parsed JSON content a GCOV_PATH file
    json_filename = os.environ['GCOV_PATH'] + '.gcov.json.gz'
    json_data = gzip.open(json_filename).read()
    return json.loads(json_data)
