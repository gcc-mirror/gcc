import os
import xml.etree.ElementTree as ET

def html_tree_from_env():
    # return parsed HTML content as an ET from an HTML_PATH file
    html_filename = os.environ['HTML_PATH']
    html_filename += '.html'
    print('html_filename: %r' % html_filename)
    return ET.parse(html_filename)
