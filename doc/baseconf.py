# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
import time
from pathlib import Path

# Build paths and add path to gcc_sphinx.py extension.
folder = Path(__file__).resolve().parent
doc_modules = folder / 'modules'
gcc_srcdir = folder / '..' / 'gcc'

sys.path.insert(0, str(doc_modules))

# gccint needs a deeper stack limit.
sys.setrecursionlimit(2000)

# -- Project information -----------------------------------------------------


def read_file(name):
    path = gcc_srcdir / name
    if path.exists():
        return open(path).read().strip()
    else:
        return ''


def __get_builder_name():
    if '-b' in sys.argv:
        return sys.argv[sys.argv.index('-b') + 1]
    else:
        return None


gcc_BASEVER = read_file('BASE-VER')
gcc_DEVPHASE = read_file('DEV-PHASE')
gcc_DATESTAMP = read_file('DATESTAMP')
gcc_REVISION = read_file('REVISION')

VERSION_PACKAGE = os.getenv('VERSION_PACKAGE')
BUGURL = os.getenv('BUGURL')
MONOCHROMATIC = os.getenv('MONOCHROMATIC')
INCLUDE_TODO = os.getenv('INCLUDE_TODO')

YEAR = time.strftime('%Y')

# The short X.Y version.
version = gcc_BASEVER

# The full version, including alpha/beta/rc tags.
release = ('%s (%s %s%s)'
           % (gcc_BASEVER, gcc_DEVPHASE, gcc_DATESTAMP,
              (' %s' % gcc_REVISION) if gcc_REVISION else ''))

rst_prolog = r'''
.. |gol| raw:: latex

               \\
.. |nbsp| unicode:: 0xA0
  :trim:
'''

needs_sphinx = '5.3'

rst_epilog = '''
.. |gcc_version| replace:: %s
.. |needs_sphinx| replace:: %s\n
.. |bugurl| replace:: %s\n
.. |package_version| replace:: %s\n
''' % (gcc_BASEVER, needs_sphinx,
       BUGURL if BUGURL else 'https://gcc.gnu.org/bugs/',
       VERSION_PACKAGE if VERSION_PACKAGE else '(GCC)')

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'gcc_sphinx',
    'sphinx.ext.intersphinx',
    'sphinx.ext.extlinks',
    'sphinx.ext.todo',
]

if __get_builder_name() == 'html':
    extensions.append('sphinx_copybutton')

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build']

# Do not highlight by default
highlight_language = 'none'

# Select C++ as a primary domain
primary_domain = 'cpp'

cpp_id_attributes = ['HOST_WIDE_INT', '__memx']

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'furo'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
html_theme_options = {
    'navigation_with_keys': True,
}

html_logo = str(folder / 'logo.svg')

html_favicon = str(folder / 'favicon.ico')

html_last_updated_fmt = ''

html_static_path = [
    str(folder / '_static')
]

html_css_files = [
    'custom.css'
]

# By default, do not generate any manual pages
man_pages = []

# FIXME: handle WARNINGs: unknown option issues and cross refs
suppress_warnings = [
    'ref.option',
]

# Use xelatex by default
latex_engine = 'xelatex'

latex_logo = str(folder / 'logo.pdf')

latex_elements = {
    'pointsize': '11pt',
    'fontpkg': r'''
\setmonofont[Scale=0.8]{DejaVu Sans Mono}
''',
    'preamble': r'''
\fvset{formatcom=\let\textbf\relax}
\protected\def\sphinxcrossref#1{#1}
''',
}

if MONOCHROMATIC:
    latex_elements['sphinxsetup'] = r'''
TitleColor={black},
InnerLinkColor={rgb}{0.0, 0.2, 0.6},
OuterLinkColor={rgb}{0.0, 0.2, 0.6},
'''

latex_table_style = ['colorrows']

# makeindex is much common on older systems
latex_use_xindy = False

texinfo_cross_references = False

# Use default as RTD theme uses default as well
pygments_style = 'bw' if MONOCHROMATIC else 'default'

option_emphasise_placeholders = True

# Ignore GitHub domain for link checking:
# https://github.com/sphinx-doc/sphinx/issues/9016
linkcheck_ignore = [
    'https://github.com/.*#.*'
]

USER_LEVEL_DOCS = ('install', 'gcc', 'gfortran', 'cpp', 'gnat_rm', 'gnat_ugn',
                   'gccgo', 'gdc', 'libgomp', 'libquadmath', 'libitm', 'libgccjit')
INTERNAL_DOCS = ('gccint', 'cppinternals', 'gfc-internals', 'gnat-style', 'libiberty')

# Cross manual reference mapping
intersphinx_mapping = {}
for manual in USER_LEVEL_DOCS + INTERNAL_DOCS:
    intersphinx_mapping[manual] = (f'https://gcc.gnu.org/onlinedocs/{manual}/', None)

# Custom references
extlinks = {
    'P': ('https://wg21.link/p%s', 'P%s'),
    'PR': ('https://gcc.gnu.org/PR%s', 'PR%s'),
    'openmp': ('https://openmp.org/specifications/#%s', 'OpenMP specification v%s'),
    'openacc': ('https://openacc.org/specification#%s', 'OpenACC specification v%s'),
}

extlinks_detect_hardcoded_links = True


# Set common settings where we need NAME of the documentation
def set_common(name, module):
    module['tags'].add(name)
    if gcc_DEVPHASE == 'experimental':
        module['todo_include_todos'] = True
        module['tags'].add('development')
    if INCLUDE_TODO:
        module['tags'].add('include_todo')

    html_theme_options['source_edit_link'] = f'https://gcc.gnu.org/onlinedocs/{name}' \
                                             '/_sources/{filename}.txt'
