# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('../share')

from adabaseconf import *

name = 'gnat_rm'
project = 'GNAT Reference Manual'
authors = 'AdaCore'

set_latex_elements(latex_elements, project)

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', f'{name}.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', name, project, authors, None, None, None, True)
]

tags.add(get_gnat_build_type())
set_common(name, globals())
