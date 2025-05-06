from sarif import *
from pprint import pprint

import pytest

@pytest.fixture(scope='function', autouse=True)
def sarif():
    return sarif_from_env()

expected_filename = 'logical-locations-2.C'

def test_logical_locations(sarif):
    runs = sarif['runs']
    run = runs[0]

    # We expect 15 logical locations within the run expressing
    # a logical hierarchy, and 8 results with one logical locations
    # in each, referencing back into the run.

    # Generate the "gold" output we expect.
    expected_in_run = []
    expected_in_results = []

    outer_ns_index_in_run = len(expected_in_run)
    expected_in_run += [{"name": "ns_outer",
                         "fullyQualifiedName": "ns_outer",
                         "kind": "namespace",
                         "index": outer_ns_index_in_run}]

    for inner_ns in ['ns_inner_1', 'ns_inner_2']:
        inner_ns_idx_in_run = len(expected_in_run)
        expected_in_run += [{"name": inner_ns,
                             "fullyQualifiedName": f"ns_outer::{inner_ns}",
                             "kind": "namespace",
                             "index": inner_ns_idx_in_run,
                             "parentIndex": 0}]
        for klass in ['klass_1', 'klass_2']:
            klass_idx_in_run = len(expected_in_run)
            expected_in_run += [{"name": klass,
                                 "kind": "type",
                                 "index": klass_idx_in_run,
                                 "parentIndex": inner_ns_idx_in_run}]
            for member_fn in ['member_fn_1', 'member_fn_2']:
                fqn = f'ns_outer::{inner_ns}::{klass}::{member_fn}'
                member_fn_idx_in_run = len(expected_in_run)
                expected_in_run += [{"name": member_fn,
                                     "kind": "function",
                                     "fullyQualifiedName": f"ns_outer::{inner_ns}::{klass}::{member_fn}",
                                     "decoratedName": f"_ZN8ns_outer10{inner_ns}7{klass}11{member_fn}Ev",
                                     "index": member_fn_idx_in_run,
                                     "parentIndex": klass_idx_in_run}]
                expected_in_results += [{'fullyQualifiedName': fqn,
                                         'index': member_fn_idx_in_run}]

    pprint(expected_in_run)
    pprint(expected_in_results)
    assert len(expected_in_run) == 15
    assert len(expected_in_results) == 8

    # We expect 15 logical locations within the run:
    assert len(run['logicalLocations']) == len(expected_in_run)
    for actual, expected in zip(run['logicalLocations'], expected_in_run):
        assert actual == expected

    # We expect 8 results with one logical location in each
    results = run['results']
    assert len(results) == len(expected_in_results)

    index = 0
    for inner_ns in ['ns_inner_1', 'ns_inner_2']:
        for klass in ['klass_1', 'klass_2']:
            for member_fn in ['member_fn_1', 'member_fn_2']:
                result = results[index]
                assert result['ruleId'] == '-fpermissive'
                assert result['level'] == 'error'
                assert result['message']['text'] \
                    == "return-statement with a value, in function returning 'void'"

                locations = result['locations']
                assert len(locations) == 1

                location = locations[0]

                # We expect one logical location within the result, referencing
                # one in the run
                assert len(location['logicalLocations']) == 1
                assert location['logicalLocations'][0] \
                    == expected_in_results[index]

                index += 1
