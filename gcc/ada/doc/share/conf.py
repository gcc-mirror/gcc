# -*- coding: utf-8 -*-
# Style_Check:Python_Fragment (meaning no pyflakes check)
#
# GNAT build configuration file

import sys
import os
import time
import re

sys.path.append('.')

import ada_pygments
import latex_elements

# Some configuration values for the various documentation handled by
# this conf.py

DOCS = {
    'gnat_rm': {
        'title': 'GNAT Reference Manual'},
    'gnat_ugn': {
        'title': 'GNAT User\'s Guide for Native Platforms'}}

# Then retrieve the source directory
root_source_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
gnatvsn_spec = os.path.join(root_source_dir, '..', 'gnatvsn.ads')
basever = os.path.join(root_source_dir, '..', '..', 'BASE-VER')
texi_fsf = True  # Set to False when FSF doc is switched to sphinx by default

with open(gnatvsn_spec, 'r') as fd:
    gnatvsn_content = fd.read()


def get_copyright():
    return '2008-%s, Free Software Foundation' % time.strftime('%Y')


def get_gnat_version():
    m = re.search(r'Gnat_Static_Version_String : ' +
                  r'constant String := "([^\(\)]+)\(.*\)?";',
                  gnatvsn_content)
    if m:
        return m.group(1).strip().decode()
    else:
        if texi_fsf and os.path.exists(basever):
            return ''

        try:
            with open(basever) as fd:
                return fd.read()
        except Exception:
            pass

    print('cannot find GNAT version in gnatvsn.ads or in ' + basever)
    sys.exit(1)


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


# First retrieve the name of the documentation we are building
doc_name = os.environ.get('DOC_NAME', None)
if doc_name is None:
    print('DOC_NAME environment variable should be set')
    sys.exit(1)

if doc_name not in DOCS:
    print('%s is not a valid documentation name' % doc_name)
    sys.exit(1)


# Exclude sources that are not part of the current documentation
exclude_patterns = []
for d in os.listdir(root_source_dir):
    if d not in ('share', doc_name, doc_name + '.rst'):
        exclude_patterns.append(d)
        print('ignoring %s' % d)

if doc_name == 'gnat_rm':
    exclude_patterns.append('share/gnat_project_manager.rst')
    print('ignoring share/gnat_project_manager.rst')

extensions = []
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = doc_name

# General information about the project.
project = DOCS[doc_name]['title']

copyright = get_copyright()

version = get_gnat_version()
release = get_gnat_version()

pygments_style = None
tags.add(get_gnat_build_type())
html_theme = 'sphinxdoc'
if os.path.isfile('adacore_transparent.png'):
    html_logo = 'adacore_transparent.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

html_static_path = ['_static']

latex_additional_files = ['gnat.sty']

copyright_macros = {
    'date': time.strftime("%b %d, %Y"),
    'edition': 'GNAT %s Edition' % 'Pro' if get_gnat_build_type() == 'PRO'
               else 'GPL',
    'name': 'GNU Ada',
    'tool': 'GNAT',
    'version': version}

latex_elements = {
    'preamble': '\\usepackage{gnat}\n' +
    latex_elements.TOC_DEPTH +
    latex_elements.PAGE_BLANK +
    latex_elements.TOC_CMD +
    latex_elements.LATEX_HYPHEN +
    latex_elements.doc_settings(DOCS[doc_name]['title'],
                                get_gnat_version()),
    'tableofcontents': latex_elements.TOC % copyright_macros}

latex_documents = [
    (master_doc, '%s.tex' % doc_name, project, 'AdaCore', 'manual')]

texinfo_documents = [
    (master_doc, doc_name, project,
     'AdaCore', doc_name, doc_name, '')]


def setup(app):
    app.add_lexer('ada', ada_pygments.AdaLexer())
    app.add_lexer('gpr', ada_pygments.GNATProjectLexer())
