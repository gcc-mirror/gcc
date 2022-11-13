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
import subprocess
import sys
# sys.path.insert(0, os.path.abspath('.'))

# gccint needs a deeper stack limit
sys.setrecursionlimit(2000)

# -- Project information -----------------------------------------------------

# The full version, including alpha/beta/rc tags

# FIXME
folder = os.path.dirname(os.path.realpath(__file__))
gcc_srcdir = os.path.join(folder, './objdir')


def __read_file(name):
    path = os.path.join(gcc_srcdir, name)
    if os.path.exists(path):
        return open(path).read().strip()
    else:
        return ''


def __get_git_revision():
    try:
        r = subprocess.check_output('git rev-parse --short HEAD', shell=True, encoding='utf8',
                                    stderr=subprocess.DEVNULL)
        return r.strip()
    except subprocess.CalledProcessError:
        return None


def __get_builder_name():
    if '-b' in sys.argv:
        return sys.argv[sys.argv.index('-b') + 1]
    else:
        return None


gcc_BASEVER = __read_file('BASE-VER')
gcc_DEVPHASE = __read_file('DEV-PHASE')
gcc_DATESTAMP = __read_file('DATESTAMP')
gcc_REVISION = __read_file('REVISION')

VERSION_PACKAGE = os.getenv('VERSION_PACKAGE', '(GCC)')
BUGURL = os.getenv('BUGURL', 'https://gcc.gnu.org/bugs/')
MONOCHROMATIC = os.getenv('MONOCHROMATIC')

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
.. |package_version| replace:: %s
.. |bugurl| replace:: %s
.. |needs_sphinx| replace:: %s
''' % (gcc_BASEVER, VERSION_PACKAGE, BUGURL, needs_sphinx)

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

html_logo = '../logo.svg'

html_favicon = '../favicon.ico'

html_last_updated_fmt = ''

html_context = {
    'commit': __get_git_revision()
}

html_static_path = [
    '../_static'
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

latex_logo = '../logo.pdf'

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

texinfo_cross_references = False

texinfo_elements = {'preamble': """
@definfoenclose strong,*,*
@definfoenclose emph,','
"""
}

# Use default as RTD theme uses default as well
pygments_style = 'bw' if MONOCHROMATIC else 'default'

option_emphasise_placeholders = True

# Ignore GitHub domain for link checking:
# https://github.com/sphinx-doc/sphinx/issues/9016
linkcheck_ignore = [
    'https://github.com/.*#.*'
]

USER_LEVEL_DOCS = ('install', 'gcc', 'gfortran', 'cpp', 'gnat_rm', 'gnat_ugn',
                   'gccgo', 'libgomp', 'libquadmath', 'libgccjit')
INTERNAL_DOCS = ('gccint', 'cppinternals', 'gfc-internals', 'gnat-style')

# Cross manual reference mapping
intersphinx_mapping = {}
for manual in USER_LEVEL_DOCS + INTERNAL_DOCS:
    intersphinx_mapping[manual] = (f'https://splichal.eu/scripts/sphinx/{manual}/_build/html/', None)

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

    html_theme_options['source_edit_link'] = f'https://splichal.eu/scripts/sphinx/{name}' \
                                               '/_build/html/_sources/{filename}.txt'
