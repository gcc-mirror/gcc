..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GET_ENVIRONMENT_VARIABLE, environment variable

.. _get_environment_variable:

GET_ENVIRONMENT_VARIABLE --- Get an environmental variable
**********************************************************

.. function:: GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)

  Get the :samp:`{VALUE}` of the environmental variable :samp:`{NAME}`.

  :param NAME:
    Shall be a scalar of type ``CHARACTER``
    and of default kind.

  :param VALUE:
    (Optional) Shall be a scalar of type ``CHARACTER``
    and of default kind.

  :param LENGTH:
    (Optional) Shall be a scalar of type ``INTEGER``
    and of default kind.

  :param STATUS:
    (Optional) Shall be a scalar of type ``INTEGER``
    and of default kind.

  :param TRIM_NAME:
    (Optional) Shall be a scalar of type ``LOGICAL``
    and of default kind.

  :return:
    Stores the value of :samp:`{NAME}` in :samp:`{VALUE}`. If :samp:`{VALUE}` is
    not large enough to hold the data, it is truncated. If :samp:`{NAME}`
    is not set, :samp:`{VALUE}` will be filled with blanks. Argument :samp:`{LENGTH}`
    contains the length needed for storing the environment variable :samp:`{NAME}`
    or zero if it is not present. :samp:`{STATUS}` is -1 if :samp:`{VALUE}` is present
    but too short for the environment variable; it is 1 if the environment
    variable does not exist and 2 if the processor does not support environment
    variables; in all other cases :samp:`{STATUS}` is zero. If :samp:`{TRIM_NAME}` is
    present with the value ``.FALSE.``, the trailing blanks in :samp:`{NAME}`
    are significant; otherwise they are not part of the environment variable
    name.

  Standard:
    Fortran 2003 and later

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GET_ENVIRONMENT_VARIABLE(NAME[, VALUE, LENGTH, STATUS, TRIM_NAME)

  Example:
    .. code-block:: fortran

      PROGRAM test_getenv
        CHARACTER(len=255) :: homedir
        CALL get_environment_variable("HOME", homedir)
        WRITE (*,*) TRIM(homedir)
      END PROGRAM