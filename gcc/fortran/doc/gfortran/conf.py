# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../../../..//doc')

from baseconf import *

name = 'gfortran'
project = 'Using GNU Fortran'
copyright = '1999-2022 Free Software Foundation, Inc.'
authors = 'The gfortran team'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', f'{name}.tex', project, authors, 'manual'),
]

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('gnu-fortran-command-options', name, 'GNU Fortran compiler', [authors], 1),
]

texinfo_documents = [
  ('index', name, project, authors, None, None, None, True)
]

set_common(name, globals())