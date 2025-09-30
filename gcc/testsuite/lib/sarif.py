import json
import os

def sarif_from_env():
    # return parsed JSON content a SARIF_PATH file
    json_filename = os.environ['SARIF_PATH']
    json_filename += '.sarif'
    print('json_filename: %r' % json_filename)
    with open(json_filename) as f:
        json_data = f.read()
    return json.loads(json_data)

def get_location_artifact_uri(location):
    return location['physicalLocation']['artifactLocation']['uri']

def get_location_physical_region(location):
    return location['physicalLocation']['region']

def get_location_snippet_text(location):
    return location['physicalLocation']['contextRegion']['snippet']['text']

def get_location_relationships(location):
    return location['relationships']

def get_result_by_index(sarif, idx):
    runs = sarif['runs']
    run = runs[0]
    results = run['results']
    return results[idx]
    
def get_state_graph(events, event_idx):
    graph = events[event_idx]['properties']['gcc/diagnostics/paths/event/state_graph']
    if 0:
        print(graph)
    assert graph is not None
    return graph

def get_state_node_attr(obj, attr_name):
    return obj['properties']['gcc/diagnostic_state_node/%s' % attr_name]

def get_state_node_kind(obj):
    return get_state_node_attr(obj, 'kind')

def get_state_node_name(obj):
    return get_state_node_attr(obj, 'name')

def get_state_node_type(obj):
    return get_state_node_attr(obj, 'type')

def get_state_node_value(obj):
    return get_state_node_attr(obj, 'value')
