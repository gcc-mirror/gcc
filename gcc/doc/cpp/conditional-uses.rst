..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _conditional-uses:

Conditional Uses
****************

There are three general reasons to use a conditional.

* A program may need to use different code depending on the machine or
  operating system it is to run on.  In some cases the code for one
  operating system may be erroneous on another operating system; for
  example, it might refer to data types or constants that do not exist on
  the other system.  When this happens, it is not enough to avoid
  executing the invalid code.  Its mere presence will cause the compiler
  to reject the program.  With a preprocessing conditional, the offending
  code can be effectively excised from the program when it is not valid.

* You may want to be able to compile the same source file into two
  different programs.  One version might make frequent time-consuming
  consistency checks on its intermediate data, or print the values of
  those data for debugging, and the other not.

* A conditional whose condition is always false is one way to exclude code
  from the program but keep it as a sort of comment for future reference.

Simple programs that do not need system-specific logic or complex
debugging hooks generally will not need to use preprocessing
conditionals.
