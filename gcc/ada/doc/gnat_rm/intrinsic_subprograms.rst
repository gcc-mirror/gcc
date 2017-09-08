.. _Intrinsic_Subprograms:

*********************
Intrinsic Subprograms
*********************

.. index:: Intrinsic Subprograms

GNAT allows a user application program to write the declaration:


.. code-block:: ada

     pragma Import (Intrinsic, name);
  

providing that the name corresponds to one of the implemented intrinsic
subprograms in GNAT, and that the parameter profile of the referenced
subprogram meets the requirements.  This chapter describes the set of
implemented intrinsic subprograms, and the requirements on parameter profiles.
Note that no body is supplied; as with other uses of pragma Import, the
body is supplied elsewhere (in this case by the compiler itself).  Note
that any use of this feature is potentially non-portable, since the
Ada standard does not require Ada compilers to implement this feature.

.. _Intrinsic_Operators:

Intrinsic Operators
===================

.. index:: Intrinsic operator

All the predefined numeric operators in package Standard
in ``pragma Import (Intrinsic,..)``
declarations.  In the binary operator case, the operands must have the same
size.  The operand or operands must also be appropriate for
the operator.  For example, for addition, the operands must
both be floating-point or both be fixed-point, and the
right operand for ``"**"`` must have a root type of
``Standard.Integer'Base``.
You can use an intrinsic operator declaration as in the following example:


.. code-block:: ada

     type Int1 is new Integer;
     type Int2 is new Integer;

     function "+" (X1 : Int1; X2 : Int2) return Int1;
     function "+" (X1 : Int1; X2 : Int2) return Int2;
     pragma Import (Intrinsic, "+");
  

This declaration would permit 'mixed mode' arithmetic on items
of the differing types ``Int1`` and ``Int2``.
It is also possible to specify such operators for private types, if the
full views are appropriate arithmetic types.

.. _Compilation_Date:

Compilation_Date
================

.. index:: Compilation_Date

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Source_Info``.  The only useful use of the
intrinsic import in this case is the one in this unit, so an
application program should simply call the function
``GNAT.Source_Info.Compilation_ISO_Date`` to obtain the date of
the current compilation (in local time format YYYY-MM-DD).

.. _Compilation_Time:

Compilation_Time
================

.. index:: Compilation_Time

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Source_Info``.  The only useful use of the
intrinsic import in this case is the one in this unit, so an
application program should simply call the function
``GNAT.Source_Info.Compilation_Time`` to obtain the time of
the current compilation (in local time format HH:MM:SS).

.. _Enclosing_Entity:

Enclosing_Entity
================

.. index:: Enclosing_Entity

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Source_Info``.  The only useful use of the
intrinsic import in this case is the one in this unit, so an
application program should simply call the function
``GNAT.Source_Info.Enclosing_Entity`` to obtain the name of
the current subprogram, package, task, entry, or protected subprogram.

.. _Exception_Information:

Exception_Information
=====================

.. index:: Exception_Information'

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Current_Exception``.  The only useful
use of the intrinsic import in this case is the one in this unit,
so an application program should simply call the function
``GNAT.Current_Exception.Exception_Information`` to obtain
the exception information associated with the current exception.

.. _Exception_Message:

Exception_Message
=================

.. index:: Exception_Message

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Current_Exception``.  The only useful
use of the intrinsic import in this case is the one in this unit,
so an application program should simply call the function
``GNAT.Current_Exception.Exception_Message`` to obtain
the message associated with the current exception.

.. _Exception_Name:

Exception_Name
==============

.. index:: Exception_Name

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Current_Exception``.  The only useful
use of the intrinsic import in this case is the one in this unit,
so an application program should simply call the function
``GNAT.Current_Exception.Exception_Name`` to obtain
the name of the current exception.

.. _File:

File
====

.. index:: File

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Source_Info``.  The only useful use of the
intrinsic import in this case is the one in this unit, so an
application program should simply call the function
``GNAT.Source_Info.File`` to obtain the name of the current
file.

.. _Line:

Line
====

.. index:: Line

This intrinsic subprogram is used in the implementation of the
library package ``GNAT.Source_Info``.  The only useful use of the
intrinsic import in this case is the one in this unit, so an
application program should simply call the function
``GNAT.Source_Info.Line`` to obtain the number of the current
source line.

.. _Shifts_and_Rotates:

Shifts and Rotates
==================

.. index:: Shift_Left

.. index:: Shift_Right

.. index:: Shift_Right_Arithmetic

.. index:: Rotate_Left

.. index:: Rotate_Right

In standard Ada, the shift and rotate functions are available only
for the predefined modular types in package ``Interfaces``.  However, in
GNAT it is possible to define these functions for any integer
type (signed or modular), as in this example:


.. code-block:: ada

     function Shift_Left
       (Value  : T;
        Amount : Natural) return T;
  

The function name must be one of
Shift_Left, Shift_Right, Shift_Right_Arithmetic, Rotate_Left, or
Rotate_Right. T must be an integer type. T'Size must be
8, 16, 32 or 64 bits; if T is modular, the modulus
must be 2**8, 2**16, 2**32 or 2**64.
The result type must be the same as the type of ``Value``.
The shift amount must be Natural.
The formal parameter names can be anything.

A more convenient way of providing these shift operators is to use
the Provide_Shift_Operators pragma, which provides the function declarations
and corresponding pragma Import's for all five shift functions.

.. _Source_Location:

Source_Location
===============

.. index:: Source_Location

This intrinsic subprogram is used in the implementation of the
library routine ``GNAT.Source_Info``.  The only useful use of the
intrinsic import in this case is the one in this unit, so an
application program should simply call the function
``GNAT.Source_Info.Source_Location`` to obtain the current
source file location.

