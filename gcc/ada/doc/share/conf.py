# -*- coding: utf-8 -*-
# Style_Check:Python_Fragment (meaning no pyflakes check)
#
# GNAT build configuration file
# --------------------------------
# This file defines the configuration for all files created
# by Sphinx. In this case, pdf (using latex) and html

import sys
import os
import time
import re

sys.path.append('.')

import ada_pygments
import latex_elements

# Define list of documents to be built and their title
DOCS = {
    "gnat_rm": {"title": "GNAT Reference Manual"},
    "gnat_ugn": {"title": "GNAT User's Guide for Native Platforms"},
    "gnat-style": {"title": "GNAT Coding Style: A Guide for GNAT Developers"},
}

# Then retrieve the source directory
root_source_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
gnatvsn_spec = os.path.join(root_source_dir, '..', 'gnatvsn.ads')
basever = os.path.join(root_source_dir, '..', '..', 'BASE-VER')
texi_fsf = True  # Set to False when FSF doc is switched to sphinx by default

# get vsn specs
with open(gnatvsn_spec, 'r') as fd:
    gnatvsn_content = fd.read()


# read copyright test from .rst file (used also for sanity-checking)
def get_copyright():
    return '2008-%s, Free Software Foundation' % time.strftime('%Y')


# get environment gnat version (used also for sanity-checking)
def get_gnat_version():
    m = re.search(r'Gnat_Static_Version_String : ' +
                  r'constant String := "([^\(\)]+)\(.*\)?";',
                  gnatvsn_content)
    if m:
        return m.group(1).strip()
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


# get gnat build type from runtime
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


# Enable Sphinx extensions
# Note that these are active for all files to be build (see DOCS list)
extensions = ['sphinx_rtd_theme']

# todo interprets ".. todo::" commands in .rst files
# mathjax enables math equations to render correctly
extensions += ['sphinx.ext.todo', 'sphinx.ext.mathjax']
todo_include_todos = True

# define templates source folder
templates_path = ['_templates']
# define the types of files to read as source for documents
source_suffix = '.rst'

# enable figure, object, table numeration on documents
print('enabling table, code-block and figure numeration')
numfig = True
numfig_format = {
    'figure': 'figure %s',
    'table': 'table %s',
    'code-block': 'listing %s',
    'section': 'section %s',
}
print('done')


# Start building the documents
# First retrieve the name of the documentation we are building
print('checking doc name... ')
doc_name = os.environ.get('DOC_NAME', None)
if doc_name is None:
    print('DOC_NAME environment variable should be set')
    sys.exit(1)

if doc_name not in DOCS:
    print('%s is not a valid documentation name' % doc_name)
    sys.exit(1)
print('found... ' , doc_name)

# Exclude sources that are not part of the current documentation
exclude_patterns = []
for d in os.listdir(root_source_dir):
    if d not in ('share', doc_name, doc_name + '.rst'):
        exclude_patterns.append(d)
        print('ignoring %s' % d)

# Special condition for gnat_rm
if doc_name == 'gnat_rm':
    exclude_patterns.append('share/gnat_project_manager.rst')
    print('ignoring share/gnat_project_manager.rst')

# General information about the project.
master_doc = doc_name
project = DOCS[doc_name]['title']

copyright = get_copyright()

version = get_gnat_version()
release = get_gnat_version()

pygments_style = None
tags.add(get_gnat_build_type())

# Define figures to be included
html_theme = 'sphinx_rtd_theme'
if os.path.isfile('adacore_transparent.png'):
    # split html and pdf logos to avoid 'same name' error in sphinx <5.2+
    html_logo = 'adacore_transparent.png'
    latex_logo = 'adacore_transparent.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

html_static_path = ['_static']

# Use gnat.sty for bulding documents
latex_additional_files = ['gnat.sty']

# Add copyright info to file
copyright_macros = {
    'date': time.strftime("%b %d, %Y"),
    'edition': 'GNAT %s Edition' % 'Pro' if get_gnat_build_type() == 'PRO'
               else 'GPL',
    'name': 'GNU Ada',
    'tool': 'GNAT',
    'version': version
}

# Send info to latex for building document
latex_elements = {
    'preamble': '\\usepackage{gnat}\n'  # use gnat.sty format
    + latex_elements.TOC_DEPTH  # define table of contents max depth to display
    + latex_elements.PAGE_BLANK  # define blank pages and when to be used
    + latex_elements.TOC_CMD  # write table of contents
    + latex_elements.LATEX_HYPHEN  # define latex hyphen '-'
    + '\\sloppy\n\n'  # sloppy/fussy define how words are spread in a paragraph
    # the following is used to send title and gnat version to latex
    + latex_elements.doc_settings(DOCS[doc_name]['title'], get_gnat_version()),
    'tableofcontents': latex_elements.TOC % copyright_macros,  # build TOC
    'papersize': 'a4paper,table',  # papersize as a4, else default letter
    'figure_align': 'H',  # align figure as square and to paragraph text
    'maketitle': '\\maketitle',  # execute custom maketitle
}

# Show page references for cross-reference in docs
latex_show_pagerefs = True
# Define latex metadata
latex_documents = [
    (master_doc, '%s.tex' % doc_name, project, 'AdaCore', 'manual')]
# Define .txt files metadata
texinfo_documents = [
    (master_doc, doc_name, project, 'AdaCore', doc_name, doc_name, '')]


# setup AdaCore custom pygments
def setup(app):
    app.add_lexer('ada', ada_pygments.AdaLexer)
    app.add_lexer('gpr', ada_pygments.GNATProjectLexer)
