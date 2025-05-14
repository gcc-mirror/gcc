from sarif import *
from pprint import pprint

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_filename = 'logical-locations-3.C'

def test_logical_locations(sarif):
    runs = sarif['runs']
    run = runs[0]

    # We expect a single error with this very long logical location:
    # ../../src/gcc/testsuite/g++.dg/sarif-output/logical-locations-3.C: In function ‘void ns_a00::ns_a01::ns_a02::ns_a03::ns_a04::ns_a05::ns_a06::ns_a07::ns_a08::ns_a09::ns_a10::ns_a11::ns_a12::ns_a13::ns_a14::ns_a15::ns_a16::ns_a17::ns_a18::ns_a19::ns_a20::ns_a21::ns_a22::ns_a23::ns_a24::ns_a25::ns_a26::ns_a27::ns_a28::ns_a29::ns_a30::ns_a31::ns_a32::ns_a33::ns_a34::ns_a35::ns_a36::ns_a37::ns_a38::ns_a39::ns_a40::ns_a41::ns_a42::ns_a43::ns_a44::ns_a45::ns_a46::ns_a47::ns_a48::ns_a49::ns_a50::ns_a51::ns_a52::ns_a53::ns_a54::ns_a55::ns_a56::ns_a57::ns_a58::ns_a59::ns_a60::ns_a61::ns_a62::ns_a63::ns_a64::ns_a65::ns_a66::ns_a67::ns_a68::ns_a69::ns_a70::ns_a71::ns_a72::ns_a73::ns_a74::ns_a75::ns_a76::ns_a77::ns_a78::ns_a79::ns_a80::ns_a81::ns_a82::ns_a83::ns_a84::ns_a85::ns_a86::ns_a87::ns_a88::ns_a89::ns_a90::ns_a91::ns_a92::ns_a93::ns_a94::ns_a95::ns_a96::ns_a97::ns_a98::ns_a99::ns_b00::ns_b01::ns_b02::ns_b03::ns_b04::ns_b05::ns_b06::ns_b07::ns_b08::ns_b09::ns_b10::ns_b11::ns_b12::ns_b13::ns_b14::ns_b15::ns_b16::ns_b17::ns_b18::ns_b19::ns_b20::ns_b21::ns_b22::ns_b23::ns_b24::ns_b25::ns_b26::ns_b27::ns_b28::ns_b29::ns_b30::ns_b31::ns_b32::ns_b33::ns_b34::ns_b35::ns_b36::ns_b37::ns_b38::ns_b39::ns_b40::ns_b41::ns_b42::ns_b43::ns_b44::ns_b45::ns_b46::ns_b47::ns_b48::ns_b49::ns_b50::ns_b51::ns_b52::ns_b53::ns_b54::ns_b55::ns_b56::ns_b57::ns_b58::ns_b59::ns_b60::ns_b61::ns_b62::ns_b63::ns_b64::ns_b65::ns_b66::ns_b67::ns_b68::ns_b69::ns_b70::ns_b71::ns_b72::ns_b73::ns_b74::ns_b75::ns_b76::ns_b77::ns_b78::ns_b79::ns_b80::ns_b81::ns_b82::ns_b83::ns_b84::ns_b85::ns_b86::ns_b87::ns_b88::ns_b89::ns_b90::ns_b91::ns_b92::ns_b93::ns_b94::ns_b95::ns_b96::ns_b97::ns_b98::ns_b99::return_from_void()’:
    # ../../src/gcc/testsuite/g++.dg/sarif-output/logical-locations-3.C:70:10: error: return-statement with a value, in function returning ‘void’ [-fpermissive]
    #   70 |   return 0;
    #      |          ^

    # We expect 201 logical locations within the run expressing
    # the logical hierarchy: the 200 nested namespaces, and then
    # the function within the innermost namespace.
    assert len(run['logicalLocations']) == 201

    outermost = run['logicalLocations'][0]
    assert outermost == {'fullyQualifiedName': 'ns_a00',
                         'index': 0,
                         'kind': 'namespace',
                         'name': 'ns_a00'}

    for i in range(1, 200):
        ns_i = run['logicalLocations'][i]
        assert ns_i['index'] == i
        assert ns_i['kind'] == 'namespace'
        assert ns_i['parentIndex'] == i - 1
        expected_name = 'ns_' + 'ab'[i // 100] + '%02i' % (i % 100)
        assert ns_i['name'] == expected_name
        assert ns_i['fullyQualifiedName'] \
            == run['logicalLocations'][i - 1]['fullyQualifiedName'] + "::" + expected_name

    innermost = run['logicalLocations'][200]
    assert innermost['index'] == 200
    assert innermost['kind'] == 'function'
    assert innermost['name'] == 'return_from_void'
    assert innermost['parentIndex'] == 199
    assert innermost['fullyQualifiedName'] \
        == run['logicalLocations'][199]['fullyQualifiedName'] + '::return_from_void'

    # We expect 1 error in the run, referring to the innermost
    # logical location by index within the run's logical locations
    results = run['results']
    assert len(results) == 1
    result = results[0]
    assert len(result['locations']) == 1
    assert len(result['locations'][0]['logicalLocations']) == 1
    assert result['locations'][0]['logicalLocations'][0]['index'] \
        == innermost['index']
    assert result['locations'][0]['logicalLocations'][0]['fullyQualifiedName'] \
        == innermost['fullyQualifiedName']
