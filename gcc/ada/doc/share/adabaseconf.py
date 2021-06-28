# GNAT build configuration file

import sys
sys.path.append('.')
sys.path.append('../../../../doc')

from baseconf import *

import os
import re
import sys
import time

import ada_latex_elements

import ada_pygments

gnatvsn_content = read_file('ada/gnatvsn.ads')


def get_gnat_version():
    m = re.search(r'Gnat_Static_Version_String : ' +
                  r'constant String := "([^\(\)]+)\(.*\)?";',
                  gnatvsn_content)
    if m:
        return m.group(1).strip()
    else:
        return gcc_BASEVER


def get_gnat_build_type():
    m = re.search(r'Build_Type : constant Gnat_Build_Type := (.+);',
                  gnatvsn_content)
    if m:
        return {'Gnatpro': 'PRO',
                'FSF': 'FSF',
                'GPL': 'GPL'}[m.group(1).strip()]
    else:
        print('cannot compute GNAT build type')
        sys.exit(1)


copyright = '2008-%s, Free Software Foundation' % YEAR

version = get_gnat_version()
release = get_gnat_version()

if os.path.isfile('adacore_transparent.png'):
    html_logo = 'adacore_transparent.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

latex_additional_files = ['../share/gnat.sty']

copyright_macros = {
    'date': time.strftime('%b %d, %Y'),
    'edition': 'GNAT %s Edition' % 'Pro' if get_gnat_build_type() == 'PRO'
               else 'GPL',
    'name': 'GNU Ada',
    'tool': 'GNAT',
    'version': version}


def set_latex_elements(latex_elements, title):
    elements = {
        'preamble': '\\usepackage{gnat}\n' +
        ada_latex_elements.TOC_DEPTH +
        ada_latex_elements.PAGE_BLANK +
        ada_latex_elements.TOC_CMD +
        ada_latex_elements.LATEX_HYPHEN +
        ada_latex_elements.doc_settings(title, get_gnat_version()),
        'tableofcontents': ada_latex_elements.TOC % copyright_macros
    }
    for key, value in elements.items():
        latex_elements.setdefault(key, '')
        latex_elements[key] += value


def setup(app):
    app.add_lexer('ada', ada_pygments.AdaLexer)
    app.add_lexer('gpr', ada_pygments.GNATProjectLexer)
