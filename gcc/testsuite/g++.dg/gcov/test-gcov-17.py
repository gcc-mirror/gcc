from gcov import gcov_from_env

import pytest


@pytest.fixture(scope='function', autouse=True)
def gcov():
    return gcov_from_env()


def test_basics(gcov):
    files = gcov['files']
    assert len(files) == 1
    functions = files[0]['functions']
    assert len(functions) == 6


def test_lines(gcov):
    lines = gcov['files'][0]['lines']
    linesdict = {}
    for line in lines:
        lineno = int(line['line_number'])
        linesdict.setdefault(lineno, [])
        linesdict[lineno].append(line)

    line9 = linesdict[9]
    assert len(line9) == 2
    assert line9[0]['function_name'] == '_ZN3FooIcE3incEv'
    assert line9[1]['function_name'] == '_ZN3FooIiE3incEv'
    assert line9[0]['count'] == 0
    assert line9[1]['count'] == 2
    assert line9[0]['unexecuted_block']
    assert not line9[1]['unexecuted_block']
    assert linesdict[36][0]['unexecuted_block']
    assert linesdict[39][0]['unexecuted_block']
    assert not linesdict[41][0]['unexecuted_block']
    assert 32 not in linesdict
    print(lines)

    line41 = linesdict[41][0]
    assert line41['count'] == 1
    assert line41['calls'][0]['returned'] == 1
    assert line41['calls'][0]['source_block_id'] == 13
    assert line41['calls'][0]['destination_block_id'] == 1
    assert len(line41['block_ids']) > 0

    line43 = linesdict[43][0]
    assert line43['count'] == 1
    assert line43['calls'][0]['returned'] == 0
