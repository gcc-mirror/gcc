..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SELECTED_CHAR_KIND, character kind, kind, character

.. _selected_char_kind:

SELECTED_CHAR_KIND --- Choose character kind
********************************************

.. function:: SELECTED_CHAR_KIND(NAME)

  ``SELECTED_CHAR_KIND(NAME)`` returns the kind value for the character
  set named :samp:`{NAME}`, if a character set with such a name is supported,
  or -1 otherwise. Currently, supported character sets include
  'ASCII' and 'DEFAULT', which are equivalent, and 'ISO_10646'
  (Universal Character Set, UCS-4) which is commonly known as Unicode.

  :param NAME:
    Shall be a scalar and of the default character type.

  Standard:
    Fortran 2003 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = SELECTED_CHAR_KIND(NAME)

  Example:
    .. code-block:: fortran

      program character_kind
        use iso_fortran_env
        implicit none
        integer, parameter :: ascii = selected_char_kind ("ascii")
        integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

        character(kind=ascii, len=26) :: alphabet
        character(kind=ucs4,  len=30) :: hello_world

        alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
        hello_world = ucs4_'Hello World and Ni Hao -- ' &
                      // char (int (z'4F60'), ucs4)     &
                      // char (int (z'597D'), ucs4)

        write (*,*) alphabet

        open (output_unit, encoding='UTF-8')
        write (*,*) trim (hello_world)
      end program character_kind