..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: extensions, not implemented

.. _extensions-not-implemented-in-gnu-fortran:

Extensions not implemented in GNU Fortran
*****************************************

The long history of the Fortran language, its wide use and broad
userbase, the large number of different compiler vendors and the lack of
some features crucial to users in the first standards have lead to the
existence of a number of important extensions to the language.  While
some of the most useful or popular extensions are supported by the GNU
Fortran compiler, not all existing extensions are supported.  This section
aims at listing these extensions and offering advice on how best make
code that uses them running with the GNU Fortran compiler.

.. More can be found here:
     - https://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/Missing-Features.html
     - the list of Fortran and libgfortran bugs closed as WONTFIX:
        http://tinyurl.com/2u4h5y

.. toctree::
  :maxdepth: 2


.. index:: ENCODE, DECODE

.. _encode-and-decode-statements:

ENCODE and DECODE statements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran does not support the ``ENCODE`` and ``DECODE``
statements.  These statements are best replaced by ``READ`` and
``WRITE`` statements involving internal files (``CHARACTER``
variables and arrays), which have been part of the Fortran standard since
Fortran 77.  For example, replace a code fragment like

.. code-block:: fortran

        INTEGER*1 LINE(80)
        REAL A, B, C
  c     ... Code that sets LINE
        DECODE (80, 9000, LINE) A, B, C
   9000 FORMAT (1X, 3(F10.5))

with the following:

.. code-block:: fortran

        CHARACTER(LEN=80) LINE
        REAL A, B, C
  c     ... Code that sets LINE
        READ (UNIT=LINE, FMT=9000) A, B, C
   9000 FORMAT (1X, 3(F10.5))

Similarly, replace a code fragment like

.. code-block:: fortran

        INTEGER*1 LINE(80)
        REAL A, B, C
  c     ... Code that sets A, B and C
        ENCODE (80, 9000, LINE) A, B, C
   9000 FORMAT (1X, 'OUTPUT IS ', 3(F10.5))

with the following:

.. code-block:: fortran

        CHARACTER(LEN=80) LINE
        REAL A, B, C
  c     ... Code that sets A, B and C
        WRITE (UNIT=LINE, FMT=9000) A, B, C
   9000 FORMAT (1X, 'OUTPUT IS ', 3(F10.5))

.. index:: FORMAT

.. _variable-format-expressions:

Variable FORMAT expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A variable ``FORMAT`` expression is format statement which includes
angle brackets enclosing a Fortran expression: ``FORMAT(I<N>)``.  GNU
Fortran does not support this legacy extension.  The effect of variable
format expressions can be reproduced by using the more powerful (and
standard) combination of internal output and string formats.  For example,
replace a code fragment like this:

.. code-block:: fortran

        WRITE(6,20) INT1
   20   FORMAT(I<N+1>)

with the following:

.. code-block:: fortran

  c     Variable declaration
        CHARACTER(LEN=20) FMT
  c
  c     Other code here...
  c
        WRITE(FMT,'("(I", I0, ")")') N+1
        WRITE(6,FMT) INT1

or with:

.. code-block:: fortran

  c     Variable declaration
        CHARACTER(LEN=20) FMT
  c
  c     Other code here...
  c
        WRITE(FMT,*) N+1
        WRITE(6,"(I" // ADJUSTL(FMT) // ")") INT1

.. index:: Complex function

.. _alternate-complex-function-syntax:

Alternate complex function syntax
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some Fortran compilers, including :command:`g77`, let the user declare
complex functions with the syntax ``COMPLEX FUNCTION name*16()``, as
well as ``COMPLEX*16 FUNCTION name()``.  Both are non-standard, legacy
extensions.  :command:`gfortran` accepts the latter form, which is more
common, but not the former.

.. index:: VOLATILE, COMMON

.. _volatile-common-blocks:

Volatile COMMON blocks
^^^^^^^^^^^^^^^^^^^^^^

Some Fortran compilers, including :command:`g77`, let the user declare
``COMMON`` with the ``VOLATILE`` attribute. This is
invalid standard Fortran syntax and is not supported by
:command:`gfortran`.  Note that :command:`gfortran` accepts
``VOLATILE`` variables in ``COMMON`` blocks since revision 4.3.

.. index:: NAME

.. _open(-...-name=):

OPEN( ... NAME=)
^^^^^^^^^^^^^^^^

Some Fortran compilers, including :command:`g77`, let the user declare
``OPEN( ... NAME=)``. This is
invalid standard Fortran syntax and is not supported by
:command:`gfortran`.  ``OPEN( ... NAME=)`` should be replaced
with ``OPEN( ... FILE=)``.

.. index:: Q edit descriptor

.. _q-edit-descriptor:

Q edit descriptor
^^^^^^^^^^^^^^^^^

Some Fortran compilers provide the ``Q`` edit descriptor, which
transfers the number of characters left within an input record into an
integer variable.

A direct replacement of the ``Q`` edit descriptor is not available
in :command:`gfortran`.  How to replicate its functionality using
standard-conforming code depends on what the intent of the original
code is.

Options to replace ``Q`` may be to read the whole line into a
character variable and then counting the number of non-blank
characters left using ``LEN_TRIM``.  Another method may be to use
formatted stream, read the data up to the position where the ``Q``
descriptor occurred, use ``INQUIRE`` to get the file position,
count the characters up to the next ``NEW_LINE`` and then start
reading from the position marked previously.
