.. _Obsolescent_Features:

********************
Obsolescent Features
********************

This chapter describes features that are provided by GNAT, but are
considered obsolescent since there are preferred ways of achieving
the same effect. These features are provided solely for historical
compatibility purposes.

.. _pragma_No_Run_Time:

pragma No_Run_Time
==================

The pragma `No_Run_Time` is used to achieve an affect similar
to the use of the "Zero Foot Print" configurable run time, but without
requiring a specially configured run time. The result of using this
pragma, which must be used for all units in a partition, is to restrict
the use of any language features requiring run-time support code. The
preferred usage is to use an appropriately configured run-time that
includes just those features that are to be made accessible.

.. _pragma_Ravenscar:

pragma Ravenscar
================

The pragma `Ravenscar` has exactly the same effect as pragma
`Profile (Ravenscar)`. The latter usage is preferred since it
is part of the new Ada 2005 standard.

.. _pragma_Restricted_Run_Time:

pragma Restricted_Run_Time
==========================

The pragma `Restricted_Run_Time` has exactly the same effect as
pragma `Profile (Restricted)`. The latter usage is
preferred since the Ada 2005 pragma `Profile` is intended for
this kind of implementation dependent addition.

.. _pragma_Task_Info:

pragma Task_Info
================

The functionality provided by pragma `Task_Info` is now part of the
Ada language. The `CPU` aspect and the package
`System.Multiprocessors` offer a less system-dependent way to specify
task affinity or to query the number of processsors.

Syntax

.. code-block:: ada

  pragma Task_Info (EXPRESSION);

This pragma appears within a task definition (like pragma
`Priority`) and applies to the task in which it appears.  The
argument must be of type `System.Task_Info.Task_Info_Type`.
The `Task_Info` pragma provides system dependent control over
aspects of tasking implementation, for example, the ability to map
tasks to specific processors.  For details on the facilities available
for the version of GNAT that you are using, see the documentation
in the spec of package System.Task_Info in the runtime
library.

.. _package_System_Task_Info:

package System.Task_Info (:file:`s-tasinf.ads`)
===============================================

This package provides target dependent functionality that is used
to support the `Task_Info` pragma. The predefined Ada package
`System.Multiprocessors` and the `CPU` aspect now provide a
standard replacement for GNAT's `Task_Info` functionality.

.. raw:: latex

    \appendix
