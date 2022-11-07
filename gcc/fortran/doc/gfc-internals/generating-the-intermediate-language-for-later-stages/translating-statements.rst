..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _translating-statements:

Translating statements
**********************

Translating statements to ``tree`` is done by functions called
``gfc_trans_*``.  These functions usually get passed a
``gfc_code`` structure, evaluate any expressions and then
return a ``tree`` structure.