# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../../..//doc')

from baseconf import *

name = 'gcc'
project = 'Using the GNU Compiler Collection'
copyright = '1988-2022 Free Software Foundation, Inc.'
authors = 'Richard M. Stallman and the GCC Developer Community'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', f'{name}.tex', project, authors, 'manual'),
]

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('gcc-command-options', 'gcc', 'GNU project C and C++ compiler', [authors], 1),
    ('gcov', 'gcov', 'coverage testing tool', [authors], 1),
    ('gcov-dump', 'gcov-dump', 'offline gcda and gcno profile dump tool', [authors], 1),
    ('gcov-tool', 'gcov-tool', 'offline gcda profile processing tool', [authors], 1),
    ('lto-dump', 'lto-dump', 'Tool for dumping LTO object files', [authors], 1),
    ('general-public-license-3', 'gpl', 'GNU General Public License', [], 7),
    ('gnu-free-documentation-license', 'gfdl', 'GNU Free Documentation License', [], 7),
    ('funding', 'fsf-funding', 'Funding Free Software', [], 7)
]

texinfo_documents = [
  ('index', name, project, authors, None, None, None, True)
]

set_common(name, globals())