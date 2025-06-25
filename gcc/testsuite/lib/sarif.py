import json
import os
import xml.etree.ElementTree as ET

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
    
def get_xml_state(events, event_idx):
    xml_src = events[event_idx]['properties']['gcc/diagnostic_event/xml_state']
    if 0:
        print(xml_src)
    xml = ET.fromstring(xml_src)
    assert xml.tag == 'state-diagram'
    return xml    
