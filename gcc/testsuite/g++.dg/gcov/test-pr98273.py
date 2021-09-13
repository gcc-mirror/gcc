from gcov import gcov_from_env

import pytest


@pytest.fixture(scope='function', autouse=True)
def gcov():
    return gcov_from_env()


def test_basics(gcov):
    files = gcov['files']
    assert len(files) == 1
    functions = files[0]['functions']
    assert len(functions) == 3


def test_lines(gcov):
    lines = gcov['files'][0]['lines']
    linesdict = {}
    for line in lines:
        linesdict[int(line['line_number'])] = line

    assert linesdict[21]['function_name'] == 'main'
    assert linesdict[15]['function_name'] == '_ZZ4mainENKUlvE_clEv'
    assert (linesdict[12]['function_name']
            == '_ZZZ4mainENKUlvE_clEvENKUlvE_clEv')
