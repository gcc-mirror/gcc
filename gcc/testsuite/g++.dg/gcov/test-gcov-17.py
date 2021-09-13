from gcov import gcov_from_env

import pytest


@pytest.fixture(scope='function', autouse=True)
def gcov():
    return gcov_from_env()


def test_basics(gcov):
    files = gcov['files']
    assert len(files) == 1
    functions = files[0]['functions']
    assert len(functions) == 5


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
    assert linesdict[31][0]['unexecuted_block']
    assert linesdict[34][0]['unexecuted_block']
    assert not linesdict[37][0]['unexecuted_block']
    assert 32 not in linesdict
