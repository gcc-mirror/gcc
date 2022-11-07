# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../../../..//doc')

from baseconf import *

name = 'gfc-internals'
project = 'GNU Fortran Internals'
copyright = '2007-2022 Free Software Foundation, Inc.'
authors = 'The gfortran team'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', f'{name}.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', name, project, authors, None, None, None, True)
]

set_common(name, globals())