.. role:: switch(samp)

.. _The_GNAT_Library:

****************
The GNAT Library
****************

The GNAT library contains a number of general and special purpose packages.
It represents functionality that the GNAT developers have found useful, and
which is made available to GNAT users.  The packages described here are fully
supported, and upwards compatibility will be maintained in future releases,
so you can use these facilities with the confidence that the same functionality
will be available in future releases.

The chapter here simply gives a brief summary of the facilities available.
The full documentation is found in the spec file for the package.  The full
sources of these library packages, including both spec and body, are provided
with all GNAT releases.  For example, to find out the full specifications of
the SPITBOL pattern matching capability, including a full tutorial and
extensive examples, look in the :file:`g-spipat.ads` file in the library.

For each entry here, the package name (as it would appear in a ``with``
clause) is given, followed by the name of the corresponding spec file in
parentheses.  The packages are children in four hierarchies, ``Ada``,
``Interfaces``, ``System``, and ``GNAT``, the latter being a
GNAT-specific hierarchy.

Note that an application program should only use packages in one of these
four hierarchies if the package is defined in the Ada Reference Manual,
or is listed in this section of the GNAT Programmers Reference Manual.
All other units should be considered internal implementation units and
should not be directly ``with``\ ed by application code.  The use of
a ``with`` clause that references one of these internal implementation
units makes an application potentially dependent on changes in versions
of GNAT, and will generate a warning message.

.. _`Ada.Characters.Latin_9_(a-chlat9.ads)`:

``Ada.Characters.Latin_9`` (:file:`a-chlat9.ads`)
=================================================

.. index:: Ada.Characters.Latin_9 (a-chlat9.ads)

.. index:: Latin_9 constants for Character

This child of ``Ada.Characters``
provides a set of definitions corresponding to those in the
RM-defined package ``Ada.Characters.Latin_1`` but with the
few modifications required for ``Latin-9``
The provision of such a package
is specifically authorized by the Ada Reference Manual
(RM A.3.3(27)).

.. _`Ada.Characters.Wide_Latin_1_(a-cwila1.ads)`:

``Ada.Characters.Wide_Latin_1`` (:file:`a-cwila1.ads`)
======================================================

.. index:: Ada.Characters.Wide_Latin_1 (a-cwila1.ads)

.. index:: Latin_1 constants for Wide_Character

This child of ``Ada.Characters``
provides a set of definitions corresponding to those in the
RM-defined package ``Ada.Characters.Latin_1`` but with the
types of the constants being ``Wide_Character``
instead of ``Character``.  The provision of such a package
is specifically authorized by the Ada Reference Manual
(RM A.3.3(27)).

.. _`Ada.Characters.Wide_Latin_9_(a-cwila1.ads)`:

``Ada.Characters.Wide_Latin_9`` (:file:`a-cwila1.ads`)
======================================================

.. index:: Ada.Characters.Wide_Latin_9 (a-cwila1.ads)

.. index:: Latin_9 constants for Wide_Character

This child of ``Ada.Characters``
provides a set of definitions corresponding to those in the
GNAT defined package ``Ada.Characters.Latin_9`` but with the
types of the constants being ``Wide_Character``
instead of ``Character``.  The provision of such a package
is specifically authorized by the Ada Reference Manual
(RM A.3.3(27)).

.. _`Ada.Characters.Wide_Wide_Latin_1_(a-chzla1.ads)`:

``Ada.Characters.Wide_Wide_Latin_1`` (:file:`a-chzla1.ads`)
===========================================================

.. index:: Ada.Characters.Wide_Wide_Latin_1 (a-chzla1.ads)

.. index:: Latin_1 constants for Wide_Wide_Character

This child of ``Ada.Characters``
provides a set of definitions corresponding to those in the
RM-defined package ``Ada.Characters.Latin_1`` but with the
types of the constants being ``Wide_Wide_Character``
instead of ``Character``.  The provision of such a package
is specifically authorized by the Ada Reference Manual
(RM A.3.3(27)).

.. _`Ada.Characters.Wide_Wide_Latin_9_(a-chzla9.ads)`:

``Ada.Characters.Wide_Wide_Latin_9`` (:file:`a-chzla9.ads`)
===========================================================

.. index:: Ada.Characters.Wide_Wide_Latin_9 (a-chzla9.ads)

.. index:: Latin_9 constants for Wide_Wide_Character

This child of ``Ada.Characters``
provides a set of definitions corresponding to those in the
GNAT defined package ``Ada.Characters.Latin_9`` but with the
types of the constants being ``Wide_Wide_Character``
instead of ``Character``.  The provision of such a package
is specifically authorized by the Ada Reference Manual
(RM A.3.3(27)).

.. _`Ada.Containers.Formal_Doubly_Linked_Lists_(a-cfdlli.ads)`:

``Ada.Containers.Formal_Doubly_Linked_Lists`` (:file:`a-cfdlli.ads`)
====================================================================

.. index:: Ada.Containers.Formal_Doubly_Linked_Lists (a-cfdlli.ads)

.. index:: Formal container for doubly linked lists

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for doubly linked lists, meant to facilitate formal
verification of code using such containers. The specification of this
unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Formal_Hashed_Maps_(a-cfhama.ads)`:

``Ada.Containers.Formal_Hashed_Maps`` (:file:`a-cfhama.ads`)
============================================================

.. index:: Ada.Containers.Formal_Hashed_Maps (a-cfhama.ads)

.. index:: Formal container for hashed maps

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for hashed maps, meant to facilitate formal
verification of code using such containers. The specification of this
unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Formal_Hashed_Sets_(a-cfhase.ads)`:

``Ada.Containers.Formal_Hashed_Sets`` (:file:`a-cfhase.ads`)
============================================================

.. index:: Ada.Containers.Formal_Hashed_Sets (a-cfhase.ads)

.. index:: Formal container for hashed sets

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for hashed sets, meant to facilitate formal
verification of code using such containers. The specification of this
unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Formal_Ordered_Maps_(a-cforma.ads)`:

``Ada.Containers.Formal_Ordered_Maps`` (:file:`a-cforma.ads`)
=============================================================

.. index:: Ada.Containers.Formal_Ordered_Maps (a-cforma.ads)

.. index:: Formal container for ordered maps

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for ordered maps, meant to facilitate formal
verification of code using such containers. The specification of this
unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Formal_Ordered_Sets_(a-cforse.ads)`:

``Ada.Containers.Formal_Ordered_Sets`` (:file:`a-cforse.ads`)
=============================================================

.. index:: Ada.Containers.Formal_Ordered_Sets (a-cforse.ads)

.. index:: Formal container for ordered sets

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for ordered sets, meant to facilitate formal
verification of code using such containers. The specification of this
unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Formal_Vectors_(a-cofove.ads)`:

``Ada.Containers.Formal_Vectors`` (:file:`a-cofove.ads`)
========================================================

.. index:: Ada.Containers.Formal_Vectors (a-cofove.ads)

.. index:: Formal container for vectors

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for vectors, meant to facilitate formal
verification of code using such containers. The specification of this
unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Formal_Indefinite_Vectors_(a-cfinve.ads)`:

``Ada.Containers.Formal_Indefinite_Vectors`` (:file:`a-cfinve.ads`)
===================================================================

.. index:: Ada.Containers.Formal_Indefinite_Vectors (a-cfinve.ads)

.. index:: Formal container for vectors

This child of ``Ada.Containers`` defines a modified version of the
Ada 2005 container for vectors of indefinite elements, meant to
facilitate formal verification of code using such containers. The
specification of this unit is compatible with SPARK 2014.

Note that although this container was designed with formal verification
in mind, it may well be generally useful in that it is a simplified more
efficient version than the one defined in the standard. In particular it
does not have the complex overhead required to detect cursor tampering.

.. _`Ada.Containers.Functional_Vectors_(a-cofuve.ads)`:

``Ada.Containers.Functional_Vectors`` (:file:`a-cofuve.ads`)
=================================================================

.. index:: Ada.Containers.Functional_Vectors (a-cofuve.ads)

.. index:: Functional vectors

This child of ``Ada.Containers`` defines immutable vectors. These
containers are unbounded and may contain indefinite elements. Furthermore, to
be usable in every context, they are neither controlled nor limited. As they
are functional, that is, no primitives are provided which would allow modifying
an existing container, these containers can still be used safely.

Their API features functions creating new containers from existing ones.
As a consequence, these containers are highly inefficient. They are also
memory consuming, as the allocated memory is not reclaimed when the container
is no longer referenced. Thus, they should in general be used in ghost code
and annotations, so that they can be removed from the final executable. The
specification of this unit is compatible with SPARK 2014.

.. _`Ada.Containers.Functional_Sets_(a-cofuse.ads)`:

``Ada.Containers.Functional_Sets`` (:file:`a-cofuse.ads`)
=================================================================

.. index:: Ada.Containers.Functional_Sets (a-cofuse.ads)

.. index:: Functional sets

This child of ``Ada.Containers`` defines immutable sets. These containers are
unbounded and may contain indefinite elements. Furthermore, to be usable in
every context, they are neither controlled nor limited. As they are functional,
that is, no primitives are provided which would allow modifying an existing
container, these containers can still be used safely.

Their API features functions creating new containers from existing ones.
As a consequence, these containers are highly inefficient. They are also
memory consuming, as the allocated memory is not reclaimed when the container
is no longer referenced. Thus, they should in general be used in ghost code
and annotations, so that they can be removed from the final executable. The
specification of this unit is compatible with SPARK 2014.

.. _`Ada.Containers.Functional_Maps_(a-cofuma.ads)`:

``Ada.Containers.Functional_Maps`` (:file:`a-cofuma.ads`)
=================================================================

.. index:: Ada.Containers.Functional_Maps (a-cofuma.ads)

.. index:: Functional maps

This child of ``Ada.Containers`` defines immutable maps. These containers are
unbounded and may contain indefinite elements. Furthermore, to be usable in
every context, they are neither controlled nor limited. As they are functional,
that is, no primitives are provided which would allow modifying an existing
container, these containers can still be used safely.

Their API features functions creating new containers from existing ones.
As a consequence, these containers are highly inefficient. They are also
memory consuming, as the allocated memory is not reclaimed when the container
is no longer referenced. Thus, they should in general be used in ghost code
and annotations, so that they can be removed from the final executable. The
specification of this unit is compatible with SPARK 2014.

.. _`Ada.Containers.Bounded_Holders_(a-coboho.ads)`:

``Ada.Containers.Bounded_Holders`` (:file:`a-coboho.ads`)
=========================================================

.. index:: Ada.Containers.Bounded_Holders (a-coboho.ads)

.. index:: Formal container for vectors

This child of ``Ada.Containers`` defines a modified version of
Indefinite_Holders that avoids heap allocation.

.. _`Ada.Command_Line.Environment_(a-colien.ads)`:

``Ada.Command_Line.Environment`` (:file:`a-colien.ads`)
=======================================================

.. index:: Ada.Command_Line.Environment (a-colien.ads)

.. index:: Environment entries

This child of ``Ada.Command_Line``
provides a mechanism for obtaining environment values on systems
where this concept makes sense.

.. _`Ada.Command_Line.Remove_(a-colire.ads)`:

``Ada.Command_Line.Remove`` (:file:`a-colire.ads`)
==================================================

.. index:: Ada.Command_Line.Remove (a-colire.ads)

.. index:: Removing command line arguments

.. index:: Command line, argument removal

This child of ``Ada.Command_Line``
provides a mechanism for logically removing
arguments from the argument list.  Once removed, an argument is not visible
to further calls on the subprograms in ``Ada.Command_Line`` will not
see the removed argument.

.. _`Ada.Command_Line.Response_File_(a-clrefi.ads)`:

``Ada.Command_Line.Response_File`` (:file:`a-clrefi.ads`)
=========================================================

.. index:: Ada.Command_Line.Response_File (a-clrefi.ads)

.. index:: Response file for command line

.. index:: Command line, response file

.. index:: Command line, handling long command lines

This child of ``Ada.Command_Line`` provides a mechanism facilities for
getting command line arguments from a text file, called a "response file".
Using a response file allow passing a set of arguments to an executable longer
than the maximum allowed by the system on the command line.

.. _`Ada.Direct_IO.C_Streams_(a-diocst.ads)`:

``Ada.Direct_IO.C_Streams`` (:file:`a-diocst.ads`)
==================================================

.. index:: Ada.Direct_IO.C_Streams (a-diocst.ads)

.. index:: C Streams, Interfacing with Direct_IO

This package provides subprograms that allow interfacing between
C streams and ``Direct_IO``.  The stream identifier can be
extracted from a file opened on the Ada side, and an Ada file
can be constructed from a stream opened on the C side.

.. _`Ada.Exceptions.Is_Null_Occurrence_(a-einuoc.ads)`:

``Ada.Exceptions.Is_Null_Occurrence`` (:file:`a-einuoc.ads`)
============================================================

.. index:: Ada.Exceptions.Is_Null_Occurrence (a-einuoc.ads)

.. index:: Null_Occurrence, testing for

This child subprogram provides a way of testing for the null
exception occurrence (``Null_Occurrence``) without raising
an exception.

.. _`Ada.Exceptions.Last_Chance_Handler_(a-elchha.ads)`:

``Ada.Exceptions.Last_Chance_Handler`` (:file:`a-elchha.ads`)
=============================================================

.. index:: Ada.Exceptions.Last_Chance_Handler (a-elchha.ads)

.. index:: Null_Occurrence, testing for

This child subprogram is used for handling otherwise unhandled
exceptions (hence the name last chance), and perform clean ups before
terminating the program. Note that this subprogram never returns.

.. _`Ada.Exceptions.Traceback_(a-exctra.ads)`:

``Ada.Exceptions.Traceback`` (:file:`a-exctra.ads`)
===================================================

.. index:: Ada.Exceptions.Traceback (a-exctra.ads)

.. index:: Traceback for Exception Occurrence

This child package provides the subprogram (``Tracebacks``) to
give a traceback array of addresses based on an exception
occurrence.

.. _`Ada.Sequential_IO.C_Streams_(a-siocst.ads)`:

``Ada.Sequential_IO.C_Streams`` (:file:`a-siocst.ads`)
======================================================

.. index:: Ada.Sequential_IO.C_Streams (a-siocst.ads)

.. index:: C Streams, Interfacing with Sequential_IO

This package provides subprograms that allow interfacing between
C streams and ``Sequential_IO``.  The stream identifier can be
extracted from a file opened on the Ada side, and an Ada file
can be constructed from a stream opened on the C side.

.. _`Ada.Streams.Stream_IO.C_Streams_(a-ssicst.ads)`:

``Ada.Streams.Stream_IO.C_Streams`` (:file:`a-ssicst.ads`)
==========================================================

.. index:: Ada.Streams.Stream_IO.C_Streams (a-ssicst.ads)

.. index:: C Streams, Interfacing with Stream_IO

This package provides subprograms that allow interfacing between
C streams and ``Stream_IO``.  The stream identifier can be
extracted from a file opened on the Ada side, and an Ada file
can be constructed from a stream opened on the C side.

.. _`Ada.Strings.Unbounded.Text_IO_(a-suteio.ads)`:

``Ada.Strings.Unbounded.Text_IO`` (:file:`a-suteio.ads`)
========================================================

.. index:: Ada.Strings.Unbounded.Text_IO (a-suteio.ads)

.. index:: Unbounded_String, IO support

.. index:: Text_IO, extensions for unbounded strings

This package provides subprograms for Text_IO for unbounded
strings, avoiding the necessity for an intermediate operation
with ordinary strings.

.. _`Ada.Strings.Wide_Unbounded.Wide_Text_IO_(a-swuwti.ads)`:

``Ada.Strings.Wide_Unbounded.Wide_Text_IO`` (:file:`a-swuwti.ads`)
==================================================================

.. index:: Ada.Strings.Wide_Unbounded.Wide_Text_IO (a-swuwti.ads)

.. index:: Unbounded_Wide_String, IO support

.. index:: Text_IO, extensions for unbounded wide strings

This package provides subprograms for Text_IO for unbounded
wide strings, avoiding the necessity for an intermediate operation
with ordinary wide strings.

.. _`Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO_(a-szuzti.ads)`:

``Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO`` (:file:`a-szuzti.ads`)
============================================================================

.. index:: Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO (a-szuzti.ads)

.. index:: Unbounded_Wide_Wide_String, IO support

.. index:: Text_IO, extensions for unbounded wide wide strings

This package provides subprograms for Text_IO for unbounded
wide wide strings, avoiding the necessity for an intermediate operation
with ordinary wide wide strings.

.. _`Ada.Text_IO.C_Streams_(a-tiocst.ads)`:

``Ada.Text_IO.C_Streams`` (:file:`a-tiocst.ads`)
================================================

.. index:: Ada.Text_IO.C_Streams (a-tiocst.ads)

.. index:: C Streams, Interfacing with ``Text_IO``

This package provides subprograms that allow interfacing between
C streams and ``Text_IO``.  The stream identifier can be
extracted from a file opened on the Ada side, and an Ada file
can be constructed from a stream opened on the C side.

.. _`Ada.Text_IO.Reset_Standard_Files_(a-tirsfi.ads)`:

``Ada.Text_IO.Reset_Standard_Files`` (:file:`a-tirsfi.ads`)
===========================================================

.. index:: Ada.Text_IO.Reset_Standard_Files (a-tirsfi.ads)

.. index:: Text_IO resetting standard files

This procedure is used to reset the status of the standard files used
by Ada.Text_IO.  This is useful in a situation (such as a restart in an
embedded application) where the status of the files may change during
execution (for example a standard input file may be redefined to be
interactive).

.. _`Ada.Wide_Characters.Unicode_(a-wichun.ads)`:

``Ada.Wide_Characters.Unicode`` (:file:`a-wichun.ads`)
======================================================

.. index:: Ada.Wide_Characters.Unicode (a-wichun.ads)

.. index:: Unicode categorization, Wide_Character

This package provides subprograms that allow categorization of
Wide_Character values according to Unicode categories.

.. _`Ada.Wide_Text_IO.C_Streams_(a-wtcstr.ads)`:

``Ada.Wide_Text_IO.C_Streams`` (:file:`a-wtcstr.ads`)
=====================================================

.. index:: Ada.Wide_Text_IO.C_Streams (a-wtcstr.ads)

.. index:: C Streams, Interfacing with ``Wide_Text_IO``

This package provides subprograms that allow interfacing between
C streams and ``Wide_Text_IO``.  The stream identifier can be
extracted from a file opened on the Ada side, and an Ada file
can be constructed from a stream opened on the C side.

.. _`Ada.Wide_Text_IO.Reset_Standard_Files_(a-wrstfi.ads)`:

``Ada.Wide_Text_IO.Reset_Standard_Files`` (:file:`a-wrstfi.ads`)
================================================================

.. index:: Ada.Wide_Text_IO.Reset_Standard_Files (a-wrstfi.ads)

.. index:: Wide_Text_IO resetting standard files

This procedure is used to reset the status of the standard files used
by Ada.Wide_Text_IO.  This is useful in a situation (such as a restart in an
embedded application) where the status of the files may change during
execution (for example a standard input file may be redefined to be
interactive).

.. _`Ada.Wide_Wide_Characters.Unicode_(a-zchuni.ads)`:

``Ada.Wide_Wide_Characters.Unicode`` (:file:`a-zchuni.ads`)
===========================================================

.. index:: Ada.Wide_Wide_Characters.Unicode (a-zchuni.ads)

.. index:: Unicode categorization, Wide_Wide_Character

This package provides subprograms that allow categorization of
Wide_Wide_Character values according to Unicode categories.

.. _`Ada.Wide_Wide_Text_IO.C_Streams_(a-ztcstr.ads)`:

``Ada.Wide_Wide_Text_IO.C_Streams`` (:file:`a-ztcstr.ads`)
==========================================================

.. index:: Ada.Wide_Wide_Text_IO.C_Streams (a-ztcstr.ads)

.. index:: C Streams, Interfacing with ``Wide_Wide_Text_IO``

This package provides subprograms that allow interfacing between
C streams and ``Wide_Wide_Text_IO``.  The stream identifier can be
extracted from a file opened on the Ada side, and an Ada file
can be constructed from a stream opened on the C side.

.. _`Ada.Wide_Wide_Text_IO.Reset_Standard_Files_(a-zrstfi.ads)`:

``Ada.Wide_Wide_Text_IO.Reset_Standard_Files`` (:file:`a-zrstfi.ads`)
=====================================================================

.. index:: Ada.Wide_Wide_Text_IO.Reset_Standard_Files (a-zrstfi.ads)

.. index:: Wide_Wide_Text_IO resetting standard files

This procedure is used to reset the status of the standard files used
by Ada.Wide_Wide_Text_IO. This is useful in a situation (such as a
restart in an embedded application) where the status of the files may
change during execution (for example a standard input file may be
redefined to be interactive).

.. _`GNAT.Altivec_(g-altive.ads)`:

``GNAT.Altivec`` (:file:`g-altive.ads`)
=======================================

.. index:: GNAT.Altivec (g-altive.ads)

.. index:: AltiVec

This is the root package of the GNAT AltiVec binding. It provides
definitions of constants and types common to all the versions of the
binding.

.. _`GNAT.Altivec.Conversions_(g-altcon.ads)`:

``GNAT.Altivec.Conversions`` (:file:`g-altcon.ads`)
===================================================

.. index:: GNAT.Altivec.Conversions (g-altcon.ads)

.. index:: AltiVec

This package provides the Vector/View conversion routines.

.. _`GNAT.Altivec.Vector_Operations_(g-alveop.ads)`:

``GNAT.Altivec.Vector_Operations`` (:file:`g-alveop.ads`)
=========================================================

.. index:: GNAT.Altivec.Vector_Operations (g-alveop.ads)

.. index:: AltiVec

This package exposes the Ada interface to the AltiVec operations on
vector objects. A soft emulation is included by default in the GNAT
library. The hard binding is provided as a separate package. This unit
is common to both bindings.

.. _`GNAT.Altivec.Vector_Types_(g-alvety.ads)`:

``GNAT.Altivec.Vector_Types`` (:file:`g-alvety.ads`)
====================================================

.. index:: GNAT.Altivec.Vector_Types (g-alvety.ads)

.. index:: AltiVec

This package exposes the various vector types part of the Ada binding
to AltiVec facilities.

.. _`GNAT.Altivec.Vector_Views_(g-alvevi.ads)`:

``GNAT.Altivec.Vector_Views`` (:file:`g-alvevi.ads`)
====================================================

.. index:: GNAT.Altivec.Vector_Views (g-alvevi.ads)

.. index:: AltiVec

This package provides public 'View' data types from/to which private
vector representations can be converted via
GNAT.Altivec.Conversions. This allows convenient access to individual
vector elements and provides a simple way to initialize vector
objects.

.. _`GNAT.Array_Split_(g-arrspl.ads)`:

``GNAT.Array_Split`` (:file:`g-arrspl.ads`)
===========================================

.. index:: GNAT.Array_Split (g-arrspl.ads)

.. index:: Array splitter

Useful array-manipulation routines: given a set of separators, split
an array wherever the separators appear, and provide direct access
to the resulting slices.

.. _`GNAT.AWK_(g-awk.ads)`:

``GNAT.AWK`` (:file:`g-awk.ads`)
================================

.. index:: GNAT.AWK (g-awk.ads)

.. index:: Parsing

.. index:: AWK

Provides AWK-like parsing functions, with an easy interface for parsing one
or more files containing formatted data.  The file is viewed as a database
where each record is a line and a field is a data element in this line.

.. _`GNAT.Bind_Environment_(g-binenv.ads)`:

``GNAT.Bind_Environment`` (:file:`g-binenv.ads`)
================================================

.. index:: GNAT.Bind_Environment (g-binenv.ads)

.. index:: Bind environment

Provides access to key=value associations captured at bind time.
These associations can be specified using the :switch:`-V` binder command
line switch.

.. _`GNAT.Bounded_Buffers_(g-boubuf.ads)`:

``GNAT.Bounded_Buffers`` (:file:`g-boubuf.ads`)
===============================================

.. index:: GNAT.Bounded_Buffers (g-boubuf.ads)

.. index:: Parsing

.. index:: Bounded Buffers

Provides a concurrent generic bounded buffer abstraction.  Instances are
useful directly or as parts of the implementations of other abstractions,
such as mailboxes.

.. _`GNAT.Bounded_Mailboxes_(g-boumai.ads)`:

``GNAT.Bounded_Mailboxes`` (:file:`g-boumai.ads`)
=================================================

.. index:: GNAT.Bounded_Mailboxes (g-boumai.ads)

.. index:: Parsing

.. index:: Mailboxes

Provides a thread-safe asynchronous intertask mailbox communication facility.

.. _`GNAT.Bubble_Sort_(g-bubsor.ads)`:

``GNAT.Bubble_Sort`` (:file:`g-bubsor.ads`)
===========================================

.. index:: GNAT.Bubble_Sort (g-bubsor.ads)

.. index:: Sorting

.. index:: Bubble sort

Provides a general implementation of bubble sort usable for sorting arbitrary
data items.  Exchange and comparison procedures are provided by passing
access-to-procedure values.

.. _`GNAT.Bubble_Sort_A_(g-busora.ads)`:

``GNAT.Bubble_Sort_A`` (:file:`g-busora.ads`)
=============================================

.. index:: GNAT.Bubble_Sort_A (g-busora.ads)

.. index:: Sorting

.. index:: Bubble sort

Provides a general implementation of bubble sort usable for sorting arbitrary
data items.  Move and comparison procedures are provided by passing
access-to-procedure values. This is an older version, retained for
compatibility. Usually ``GNAT.Bubble_Sort`` will be preferable.

.. _`GNAT.Bubble_Sort_G_(g-busorg.ads)`:

``GNAT.Bubble_Sort_G`` (:file:`g-busorg.ads`)
=============================================

.. index:: GNAT.Bubble_Sort_G (g-busorg.ads)

.. index:: Sorting

.. index:: Bubble sort

Similar to ``Bubble_Sort_A`` except that the move and sorting procedures
are provided as generic parameters, this improves efficiency, especially
if the procedures can be inlined, at the expense of duplicating code for
multiple instantiations.

.. _`GNAT.Byte_Order_Mark_(g-byorma.ads)`:

``GNAT.Byte_Order_Mark`` (:file:`g-byorma.ads`)
===============================================

.. index:: GNAT.Byte_Order_Mark (g-byorma.ads)

.. index:: UTF-8 representation

.. index:: Wide characte representations

Provides a routine which given a string, reads the start of the string to
see whether it is one of the standard byte order marks (BOM's) which signal
the encoding of the string. The routine includes detection of special XML
sequences for various UCS input formats.

.. _`GNAT.Byte_Swapping_(g-bytswa.ads)`:

``GNAT.Byte_Swapping`` (:file:`g-bytswa.ads`)
=============================================

.. index:: GNAT.Byte_Swapping (g-bytswa.ads)

.. index:: Byte swapping

.. index:: Endianness

General routines for swapping the bytes in 2-, 4-, and 8-byte quantities.
Machine-specific implementations are available in some cases.

.. _`GNAT.Calendar_(g-calend.ads)`:

``GNAT.Calendar`` (:file:`g-calend.ads`)
========================================

.. index:: GNAT.Calendar (g-calend.ads)

.. index:: Calendar

Extends the facilities provided by ``Ada.Calendar`` to include handling
of days of the week, an extended ``Split`` and ``Time_Of`` capability.
Also provides conversion of ``Ada.Calendar.Time`` values to and from the
C ``timeval`` format.

.. _`GNAT.Calendar.Time_IO_(g-catiio.ads)`:

``GNAT.Calendar.Time_IO`` (:file:`g-catiio.ads`)
================================================

.. index:: Calendar

.. index:: Time

.. index:: GNAT.Calendar.Time_IO (g-catiio.ads)

.. _`GNAT.CRC32_(g-crc32.ads)`:

``GNAT.CRC32`` (:file:`g-crc32.ads`)
====================================

.. index:: GNAT.CRC32 (g-crc32.ads)

.. index:: CRC32

.. index:: Cyclic Redundancy Check

This package implements the CRC-32 algorithm.  For a full description
of this algorithm see
*Computation of Cyclic Redundancy Checks via Table Look-Up*,
:title:`Communications of the ACM`, Vol. 31 No. 8, pp. 1008-1013,
Aug. 1988.  Sarwate, D.V.

.. _`GNAT.Case_Util_(g-casuti.ads)`:

``GNAT.Case_Util`` (:file:`g-casuti.ads`)
=========================================

.. index:: GNAT.Case_Util (g-casuti.ads)

.. index:: Casing utilities

.. index:: Character handling (``GNAT.Case_Util``)

A set of simple routines for handling upper and lower casing of strings
without the overhead of the full casing tables
in ``Ada.Characters.Handling``.

.. _`GNAT.CGI_(g-cgi.ads)`:

``GNAT.CGI`` (:file:`g-cgi.ads`)
================================

.. index:: GNAT.CGI (g-cgi.ads)

.. index:: CGI (Common Gateway Interface)

This is a package for interfacing a GNAT program with a Web server via the
Common Gateway Interface (CGI).  Basically this package parses the CGI
parameters, which are a set of key/value pairs sent by the Web server.  It
builds a table whose index is the key and provides some services to deal
with this table.

.. _`GNAT.CGI.Cookie_(g-cgicoo.ads)`:

``GNAT.CGI.Cookie`` (:file:`g-cgicoo.ads`)
==========================================

.. index:: GNAT.CGI.Cookie (g-cgicoo.ads)

.. index:: CGI (Common Gateway Interface) cookie support

.. index:: Cookie support in CGI

This is a package to interface a GNAT program with a Web server via the
Common Gateway Interface (CGI).  It exports services to deal with Web
cookies (piece of information kept in the Web client software).

.. _`GNAT.CGI.Debug_(g-cgideb.ads)`:

``GNAT.CGI.Debug`` (:file:`g-cgideb.ads`)
=========================================

.. index:: GNAT.CGI.Debug (g-cgideb.ads)

.. index:: CGI (Common Gateway Interface) debugging

This is a package to help debugging CGI (Common Gateway Interface)
programs written in Ada.

.. _`GNAT.Command_Line_(g-comlin.ads)`:

``GNAT.Command_Line`` (:file:`g-comlin.ads`)
============================================

.. index:: GNAT.Command_Line (g-comlin.ads)

.. index:: Command line

Provides a high level interface to ``Ada.Command_Line`` facilities,
including the ability to scan for named switches with optional parameters
and expand file names using wild card notations.

.. _`GNAT.Compiler_Version_(g-comver.ads)`:

``GNAT.Compiler_Version`` (:file:`g-comver.ads`)
================================================

.. index:: GNAT.Compiler_Version (g-comver.ads)

.. index:: Compiler Version

.. index:: Version, of compiler

Provides a routine for obtaining the version of the compiler used to
compile the program. More accurately this is the version of the binder
used to bind the program (this will normally be the same as the version
of the compiler if a consistent tool set is used to compile all units
of a partition).

.. _`GNAT.Ctrl_C_(g-ctrl_c.ads)`:

``GNAT.Ctrl_C`` (:file:`g-ctrl_c.ads`)
======================================

.. index:: GNAT.Ctrl_C (g-ctrl_c.ads)

.. index:: Interrupt

Provides a simple interface to handle Ctrl-C keyboard events.

.. _`GNAT.Current_Exception_(g-curexc.ads)`:

``GNAT.Current_Exception`` (:file:`g-curexc.ads`)
=================================================

.. index:: GNAT.Current_Exception (g-curexc.ads)

.. index:: Current exception

.. index:: Exception retrieval

Provides access to information on the current exception that has been raised
without the need for using the Ada 95 / Ada 2005 exception choice parameter
specification syntax.
This is particularly useful in simulating typical facilities for
obtaining information about exceptions provided by Ada 83 compilers.

.. _`GNAT.Debug_Pools_(g-debpoo.ads)`:

``GNAT.Debug_Pools`` (:file:`g-debpoo.ads`)
===========================================

.. index:: GNAT.Debug_Pools (g-debpoo.ads)

.. index:: Debugging

.. index:: Debug pools

.. index:: Memory corruption debugging

Provide a debugging storage pools that helps tracking memory corruption
problems.
See ``The GNAT Debug_Pool Facility`` section in the :title:`GNAT User's Guide`.

.. _`GNAT.Debug_Utilities_(g-debuti.ads)`:

``GNAT.Debug_Utilities`` (:file:`g-debuti.ads`)
===============================================

.. index:: GNAT.Debug_Utilities (g-debuti.ads)

.. index:: Debugging

Provides a few useful utilities for debugging purposes, including conversion
to and from string images of address values. Supports both C and Ada formats
for hexadecimal literals.

.. _`GNAT.Decode_String_(g-decstr.ads)`:

``GNAT.Decode_String`` (:file:`g-decstr.ads`)
=============================================

.. index:: GNAT.Decode_String (g-decstr.ads)

.. index:: Decoding strings

.. index:: String decoding

.. index:: Wide character encoding

.. index:: UTF-8

.. index:: Unicode

A generic package providing routines for decoding wide character and wide wide
character strings encoded as sequences of 8-bit characters using a specified
encoding method. Includes validation routines, and also routines for stepping
to next or previous encoded character in an encoded string.
Useful in conjunction with Unicode character coding. Note there is a
preinstantiation for UTF-8. See next entry.

.. _`GNAT.Decode_UTF8_String_(g-deutst.ads)`:

``GNAT.Decode_UTF8_String`` (:file:`g-deutst.ads`)
==================================================

.. index:: GNAT.Decode_UTF8_String (g-deutst.ads)

.. index:: Decoding strings

.. index:: Decoding UTF-8 strings

.. index:: UTF-8 string decoding

.. index:: Wide character decoding

.. index:: UTF-8

.. index:: Unicode

A preinstantiation of GNAT.Decode_Strings for UTF-8 encoding.

.. _`GNAT.Directory_Operations_(g-dirope.ads)`:

``GNAT.Directory_Operations`` (:file:`g-dirope.ads`)
====================================================

.. index:: GNAT.Directory_Operations (g-dirope.ads)

.. index:: Directory operations

Provides a set of routines for manipulating directories, including changing
the current directory, making new directories, and scanning the files in a
directory.

.. _`GNAT.Directory_Operations.Iteration_(g-diopit.ads)`:

``GNAT.Directory_Operations.Iteration`` (:file:`g-diopit.ads`)
==============================================================

.. index:: GNAT.Directory_Operations.Iteration (g-diopit.ads)

.. index:: Directory operations iteration

A child unit of GNAT.Directory_Operations providing additional operations
for iterating through directories.

.. _`GNAT.Dynamic_HTables_(g-dynhta.ads)`:

``GNAT.Dynamic_HTables`` (:file:`g-dynhta.ads`)
===============================================

.. index:: GNAT.Dynamic_HTables (g-dynhta.ads)

.. index:: Hash tables

A generic implementation of hash tables that can be used to hash arbitrary
data.  Provided in two forms, a simple form with built in hash functions,
and a more complex form in which the hash function is supplied.

This package provides a facility similar to that of ``GNAT.HTable``,
except that this package declares a type that can be used to define
dynamic instances of the hash table, while an instantiation of
``GNAT.HTable`` creates a single instance of the hash table.

.. _`GNAT.Dynamic_Tables_(g-dyntab.ads)`:

``GNAT.Dynamic_Tables`` (:file:`g-dyntab.ads`)
==============================================

.. index:: GNAT.Dynamic_Tables (g-dyntab.ads)

.. index:: Table implementation

.. index:: Arrays, extendable

A generic package providing a single dimension array abstraction where the
length of the array can be dynamically modified.

This package provides a facility similar to that of ``GNAT.Table``,
except that this package declares a type that can be used to define
dynamic instances of the table, while an instantiation of
``GNAT.Table`` creates a single instance of the table type.

.. _`GNAT.Encode_String_(g-encstr.ads)`:

``GNAT.Encode_String`` (:file:`g-encstr.ads`)
=============================================

.. index:: GNAT.Encode_String (g-encstr.ads)

.. index:: Encoding strings

.. index:: String encoding

.. index:: Wide character encoding

.. index:: UTF-8

.. index:: Unicode

A generic package providing routines for encoding wide character and wide
wide character strings as sequences of 8-bit characters using a specified
encoding method. Useful in conjunction with Unicode character coding.
Note there is a preinstantiation for UTF-8. See next entry.

.. _`GNAT.Encode_UTF8_String_(g-enutst.ads)`:

``GNAT.Encode_UTF8_String`` (:file:`g-enutst.ads`)
==================================================

.. index:: GNAT.Encode_UTF8_String (g-enutst.ads)

.. index:: Encoding strings

.. index:: Encoding UTF-8 strings

.. index:: UTF-8 string encoding

.. index:: Wide character encoding

.. index:: UTF-8

.. index:: Unicode

A preinstantiation of GNAT.Encode_Strings for UTF-8 encoding.

.. _`GNAT.Exception_Actions_(g-excact.ads)`:

``GNAT.Exception_Actions`` (:file:`g-excact.ads`)
=================================================

.. index:: GNAT.Exception_Actions (g-excact.ads)

.. index:: Exception actions

Provides callbacks when an exception is raised. Callbacks can be registered
for specific exceptions, or when any exception is raised. This
can be used for instance to force a core dump to ease debugging.

.. _`GNAT.Exception_Traces_(g-exctra.ads)`:

``GNAT.Exception_Traces`` (:file:`g-exctra.ads`)
================================================

.. index:: GNAT.Exception_Traces (g-exctra.ads)

.. index:: Exception traces

.. index:: Debugging

Provides an interface allowing to control automatic output upon exception
occurrences.

.. _`GNAT.Exceptions_(g-expect.ads)`:

``GNAT.Exceptions`` (:file:`g-expect.ads`)
==========================================

.. index:: GNAT.Exceptions (g-expect.ads)

.. index:: Exceptions, Pure

.. index:: Pure packages, exceptions

Normally it is not possible to raise an exception with
a message from a subprogram in a pure package, since the
necessary types and subprograms are in ``Ada.Exceptions``
which is not a pure unit. ``GNAT.Exceptions`` provides a
facility for getting around this limitation for a few
predefined exceptions, and for example allow raising
``Constraint_Error`` with a message from a pure subprogram.

.. _`GNAT.Expect_(g-expect.ads)`:

``GNAT.Expect`` (:file:`g-expect.ads`)
======================================

.. index:: GNAT.Expect (g-expect.ads)

Provides a set of subprograms similar to what is available
with the standard Tcl Expect tool.
It allows you to easily spawn and communicate with an external process.
You can send commands or inputs to the process, and compare the output
with some expected regular expression. Currently ``GNAT.Expect``
is implemented on all native GNAT ports.
It is not implemented for cross ports, and in particular is not
implemented for VxWorks or LynxOS.

.. _`GNAT.Expect.TTY_(g-exptty.ads)`:

``GNAT.Expect.TTY`` (:file:`g-exptty.ads`)
==========================================

.. index:: GNAT.Expect.TTY (g-exptty.ads)

As GNAT.Expect but using pseudo-terminal.
Currently ``GNAT.Expect.TTY`` is implemented on all native GNAT
ports. It is not implemented for cross ports, and
in particular is not implemented for VxWorks or LynxOS.

.. _`GNAT.Float_Control_(g-flocon.ads)`:

``GNAT.Float_Control`` (:file:`g-flocon.ads`)
=============================================

.. index:: GNAT.Float_Control (g-flocon.ads)

.. index:: Floating-Point Processor

Provides an interface for resetting the floating-point processor into the
mode required for correct semantic operation in Ada.  Some third party
library calls may cause this mode to be modified, and the Reset procedure
in this package can be used to reestablish the required mode.

.. _`GNAT.Formatted_String_(g-forstr.ads)`:

``GNAT.Formatted_String`` (:file:`g-forstr.ads`)
================================================

.. index:: GNAT.Formatted_String (g-forstr.ads)

.. index:: Formatted String

Provides support for C/C++ printf() formatted strings. The format is
copied from the printf() routine and should therefore gives identical
output. Some generic routines are provided to be able to use types
derived from Integer, Float or enumerations as values for the
formatted string.

.. _`GNAT.Heap_Sort_(g-heasor.ads)`:

``GNAT.Heap_Sort`` (:file:`g-heasor.ads`)
=========================================

.. index:: GNAT.Heap_Sort (g-heasor.ads)

.. index:: Sorting

Provides a general implementation of heap sort usable for sorting arbitrary
data items. Exchange and comparison procedures are provided by passing
access-to-procedure values.  The algorithm used is a modified heap sort
that performs approximately N*log(N) comparisons in the worst case.

.. _`GNAT.Heap_Sort_A_(g-hesora.ads)`:

``GNAT.Heap_Sort_A`` (:file:`g-hesora.ads`)
===========================================

.. index:: GNAT.Heap_Sort_A (g-hesora.ads)

.. index:: Sorting

Provides a general implementation of heap sort usable for sorting arbitrary
data items. Move and comparison procedures are provided by passing
access-to-procedure values.  The algorithm used is a modified heap sort
that performs approximately N*log(N) comparisons in the worst case.
This differs from ``GNAT.Heap_Sort`` in having a less convenient
interface, but may be slightly more efficient.

.. _`GNAT.Heap_Sort_G_(g-hesorg.ads)`:

``GNAT.Heap_Sort_G`` (:file:`g-hesorg.ads`)
===========================================

.. index:: GNAT.Heap_Sort_G (g-hesorg.ads)

.. index:: Sorting

Similar to ``Heap_Sort_A`` except that the move and sorting procedures
are provided as generic parameters, this improves efficiency, especially
if the procedures can be inlined, at the expense of duplicating code for
multiple instantiations.

.. _`GNAT.HTable_(g-htable.ads)`:

``GNAT.HTable`` (:file:`g-htable.ads`)
======================================

.. index:: GNAT.HTable (g-htable.ads)

.. index:: Hash tables

A generic implementation of hash tables that can be used to hash arbitrary
data.  Provides two approaches, one a simple static approach, and the other
allowing arbitrary dynamic hash tables.

.. _`GNAT.IO_(g-io.ads)`:

``GNAT.IO`` (:file:`g-io.ads`)
==============================

.. index:: GNAT.IO (g-io.ads)

.. index:: Simple I/O

.. index:: Input/Output facilities

A simple preelaborable input-output package that provides a subset of
simple Text_IO functions for reading characters and strings from
Standard_Input, and writing characters, strings and integers to either
Standard_Output or Standard_Error.

.. _`GNAT.IO_Aux_(g-io_aux.ads)`:

``GNAT.IO_Aux`` (:file:`g-io_aux.ads`)
======================================

.. index:: GNAT.IO_Aux (g-io_aux.ads)

.. index:: Text_IO

.. index:: Input/Output facilities

Provides some auxiliary functions for use with Text_IO, including a test
for whether a file exists, and functions for reading a line of text.

.. _`GNAT.Lock_Files_(g-locfil.ads)`:

``GNAT.Lock_Files`` (:file:`g-locfil.ads`)
==========================================

.. index:: GNAT.Lock_Files (g-locfil.ads)

.. index:: File locking

.. index:: Locking using files

Provides a general interface for using files as locks.  Can be used for
providing program level synchronization.

.. _`GNAT.MBBS_Discrete_Random_(g-mbdira.ads)`:

``GNAT.MBBS_Discrete_Random`` (:file:`g-mbdira.ads`)
====================================================

.. index:: GNAT.MBBS_Discrete_Random (g-mbdira.ads)

.. index:: Random number generation

The original implementation of ``Ada.Numerics.Discrete_Random``.  Uses
a modified version of the Blum-Blum-Shub generator.

.. _`GNAT.MBBS_Float_Random_(g-mbflra.ads)`:

``GNAT.MBBS_Float_Random`` (:file:`g-mbflra.ads`)
=================================================

.. index:: GNAT.MBBS_Float_Random (g-mbflra.ads)

.. index:: Random number generation

The original implementation of ``Ada.Numerics.Float_Random``.  Uses
a modified version of the Blum-Blum-Shub generator.

.. _`GNAT.MD5_(g-md5.ads)`:

``GNAT.MD5`` (:file:`g-md5.ads`)
================================

.. index:: GNAT.MD5 (g-md5.ads)

.. index:: Message Digest MD5

Implements the MD5 Message-Digest Algorithm as described in RFC 1321, and
the HMAC-MD5 message authentication function as described in RFC 2104 and
FIPS PUB 198.

.. _`GNAT.Memory_Dump_(g-memdum.ads)`:

``GNAT.Memory_Dump`` (:file:`g-memdum.ads`)
===========================================

.. index:: GNAT.Memory_Dump (g-memdum.ads)

.. index:: Dump Memory

Provides a convenient routine for dumping raw memory to either the
standard output or standard error files. Uses GNAT.IO for actual
output.

.. _`GNAT.Most_Recent_Exception_(g-moreex.ads)`:

``GNAT.Most_Recent_Exception`` (:file:`g-moreex.ads`)
=====================================================

.. index:: GNAT.Most_Recent_Exception (g-moreex.ads)

.. index:: Exception, obtaining most recent

Provides access to the most recently raised exception.  Can be used for
various logging purposes, including duplicating functionality of some
Ada 83 implementation dependent extensions.

.. _`GNAT.OS_Lib_(g-os_lib.ads)`:

``GNAT.OS_Lib`` (:file:`g-os_lib.ads`)
======================================

.. index:: GNAT.OS_Lib (g-os_lib.ads)

.. index:: Operating System interface

.. index:: Spawn capability

Provides a range of target independent operating system interface functions,
including time/date management, file operations, subprocess management,
including a portable spawn procedure, and access to environment variables
and error return codes.

.. _`GNAT.Perfect_Hash_Generators_(g-pehage.ads)`:

``GNAT.Perfect_Hash_Generators`` (:file:`g-pehage.ads`)
=======================================================

.. index:: GNAT.Perfect_Hash_Generators (g-pehage.ads)

.. index:: Hash functions

Provides a generator of static minimal perfect hash functions. No
collisions occur and each item can be retrieved from the table in one
probe (perfect property). The hash table size corresponds to the exact
size of the key set and no larger (minimal property). The key set has to
be know in advance (static property). The hash functions are also order
preserving. If w2 is inserted after w1 in the generator, their
hashcode are in the same order. These hashing functions are very
convenient for use with realtime applications.

.. _`GNAT.Random_Numbers_(g-rannum.ads)`:

``GNAT.Random_Numbers`` (:file:`g-rannum.ads`)
==============================================

.. index:: GNAT.Random_Numbers (g-rannum.ads)

.. index:: Random number generation

Provides random number capabilities which extend those available in the
standard Ada library and are more convenient to use.

.. _`GNAT.Regexp_(g-regexp.ads)`:

``GNAT.Regexp`` (:file:`g-regexp.ads`)
======================================

.. index:: GNAT.Regexp (g-regexp.ads)

.. index:: Regular expressions

.. index:: Pattern matching

A simple implementation of regular expressions, using a subset of regular
expression syntax copied from familiar Unix style utilities.  This is the
simplest of the three pattern matching packages provided, and is particularly
suitable for 'file globbing' applications.

.. _`GNAT.Registry_(g-regist.ads)`:

``GNAT.Registry`` (:file:`g-regist.ads`)
========================================

.. index:: GNAT.Registry (g-regist.ads)

.. index:: Windows Registry

This is a high level binding to the Windows registry.  It is possible to
do simple things like reading a key value, creating a new key.  For full
registry API, but at a lower level of abstraction, refer to the Win32.Winreg
package provided with the Win32Ada binding

.. _`GNAT.Regpat_(g-regpat.ads)`:

``GNAT.Regpat`` (:file:`g-regpat.ads`)
======================================

.. index:: GNAT.Regpat (g-regpat.ads)

.. index:: Regular expressions

.. index:: Pattern matching

A complete implementation of Unix-style regular expression matching, copied
from the original V7 style regular expression library written in C by
Henry Spencer (and binary compatible with this C library).

.. _`GNAT.Rewrite_Data_(g-rewdat.ads)`:

``GNAT.Rewrite_Data`` (:file:`g-rewdat.ads`)
============================================

.. index:: GNAT.Rewrite_Data (g-rewdat.ads)

.. index:: Rewrite data

A unit to rewrite on-the-fly string occurrences in a stream of
data. The implementation has a very minimal memory footprint as the
full content to be processed is not loaded into memory all at once. This makes
this interface usable for large files or socket streams.

.. _`GNAT.Secondary_Stack_Info_(g-sestin.ads)`:

``GNAT.Secondary_Stack_Info`` (:file:`g-sestin.ads`)
====================================================

.. index:: GNAT.Secondary_Stack_Info (g-sestin.ads)

.. index:: Secondary Stack Info

Provide the capability to query the high water mark of the current task's
secondary stack.

.. _`GNAT.Semaphores_(g-semaph.ads)`:

``GNAT.Semaphores`` (:file:`g-semaph.ads`)
==========================================

.. index:: GNAT.Semaphores (g-semaph.ads)

.. index:: Semaphores

Provides classic counting and binary semaphores using protected types.

.. _`GNAT.Serial_Communications_(g-sercom.ads)`:

``GNAT.Serial_Communications`` (:file:`g-sercom.ads`)
=====================================================

.. index:: GNAT.Serial_Communications (g-sercom.ads)

.. index:: Serial_Communications

Provides a simple interface to send and receive data over a serial
port. This is only supported on GNU/Linux and Windows.

.. _`GNAT.SHA1_(g-sha1.ads)`:

``GNAT.SHA1`` (:file:`g-sha1.ads`)
==================================

.. index:: GNAT.SHA1 (g-sha1.ads)

.. index:: Secure Hash Algorithm SHA-1

Implements the SHA-1 Secure Hash Algorithm as described in FIPS PUB 180-3
and RFC 3174, and the HMAC-SHA1 message authentication function as described
in RFC 2104 and FIPS PUB 198.

.. _`GNAT.SHA224_(g-sha224.ads)`:

``GNAT.SHA224`` (:file:`g-sha224.ads`)
======================================

.. index:: GNAT.SHA224 (g-sha224.ads)

.. index:: Secure Hash Algorithm SHA-224

Implements the SHA-224 Secure Hash Algorithm as described in FIPS PUB 180-3,
and the HMAC-SHA224 message authentication function as described
in RFC 2104 and FIPS PUB 198.

.. _`GNAT.SHA256_(g-sha256.ads)`:

``GNAT.SHA256`` (:file:`g-sha256.ads`)
======================================

.. index:: GNAT.SHA256 (g-sha256.ads)

.. index:: Secure Hash Algorithm SHA-256

Implements the SHA-256 Secure Hash Algorithm as described in FIPS PUB 180-3,
and the HMAC-SHA256 message authentication function as described
in RFC 2104 and FIPS PUB 198.

.. _`GNAT.SHA384_(g-sha384.ads)`:

``GNAT.SHA384`` (:file:`g-sha384.ads`)
======================================

.. index:: GNAT.SHA384 (g-sha384.ads)

.. index:: Secure Hash Algorithm SHA-384

Implements the SHA-384 Secure Hash Algorithm as described in FIPS PUB 180-3,
and the HMAC-SHA384 message authentication function as described
in RFC 2104 and FIPS PUB 198.

.. _`GNAT.SHA512_(g-sha512.ads)`:

``GNAT.SHA512`` (:file:`g-sha512.ads`)
======================================

.. index:: GNAT.SHA512 (g-sha512.ads)

.. index:: Secure Hash Algorithm SHA-512

Implements the SHA-512 Secure Hash Algorithm as described in FIPS PUB 180-3,
and the HMAC-SHA512 message authentication function as described
in RFC 2104 and FIPS PUB 198.

.. _`GNAT.Signals_(g-signal.ads)`:

``GNAT.Signals`` (:file:`g-signal.ads`)
=======================================

.. index:: GNAT.Signals (g-signal.ads)

.. index:: Signals

Provides the ability to manipulate the blocked status of signals on supported
targets.

.. _`GNAT.Sockets_(g-socket.ads)`:

``GNAT.Sockets`` (:file:`g-socket.ads`)
=======================================

.. index:: GNAT.Sockets (g-socket.ads)

.. index:: Sockets

A high level and portable interface to develop sockets based applications.
This package is based on the sockets thin binding found in
``GNAT.Sockets.Thin``. Currently ``GNAT.Sockets`` is implemented
on all native GNAT ports and on VxWorks cross prots.  It is not implemented for
the LynxOS cross port.

.. _`GNAT.Source_Info_(g-souinf.ads)`:

``GNAT.Source_Info`` (:file:`g-souinf.ads`)
===========================================

.. index:: GNAT.Source_Info (g-souinf.ads)

.. index:: Source Information

Provides subprograms that give access to source code information known at
compile time, such as the current file name and line number. Also provides
subprograms yielding the date and time of the current compilation (like the
C macros ``__DATE__`` and ``__TIME__``)

.. _`GNAT.Spelling_Checker_(g-speche.ads)`:

``GNAT.Spelling_Checker`` (:file:`g-speche.ads`)
================================================

.. index:: GNAT.Spelling_Checker (g-speche.ads)

.. index:: Spell checking

Provides a function for determining whether one string is a plausible
near misspelling of another string.

.. _`GNAT.Spelling_Checker_Generic_(g-spchge.ads)`:

``GNAT.Spelling_Checker_Generic`` (:file:`g-spchge.ads`)
========================================================

.. index:: GNAT.Spelling_Checker_Generic (g-spchge.ads)

.. index:: Spell checking

Provides a generic function that can be instantiated with a string type for
determining whether one string is a plausible near misspelling of another
string.

.. _`GNAT.Spitbol.Patterns_(g-spipat.ads)`:

``GNAT.Spitbol.Patterns`` (:file:`g-spipat.ads`)
================================================

.. index:: GNAT.Spitbol.Patterns (g-spipat.ads)

.. index:: SPITBOL pattern matching

.. index:: Pattern matching

A complete implementation of SNOBOL4 style pattern matching.  This is the
most elaborate of the pattern matching packages provided.  It fully duplicates
the SNOBOL4 dynamic pattern construction and matching capabilities, using the
efficient algorithm developed by Robert Dewar for the SPITBOL system.

.. _`GNAT.Spitbol_(g-spitbo.ads)`:

``GNAT.Spitbol`` (:file:`g-spitbo.ads`)
=======================================

.. index:: GNAT.Spitbol (g-spitbo.ads)

.. index:: SPITBOL interface

The top level package of the collection of SPITBOL-style functionality, this
package provides basic SNOBOL4 string manipulation functions, such as
Pad, Reverse, Trim, Substr capability, as well as a generic table function
useful for constructing arbitrary mappings from strings in the style of
the SNOBOL4 TABLE function.

.. _`GNAT.Spitbol.Table_Boolean_(g-sptabo.ads)`:

``GNAT.Spitbol.Table_Boolean`` (:file:`g-sptabo.ads`)
=====================================================

.. index:: GNAT.Spitbol.Table_Boolean (g-sptabo.ads)

.. index:: Sets of strings

.. index:: SPITBOL Tables

A library level of instantiation of ``GNAT.Spitbol.Patterns.Table``
for type ``Standard.Boolean``, giving an implementation of sets of
string values.

.. _`GNAT.Spitbol.Table_Integer_(g-sptain.ads)`:

``GNAT.Spitbol.Table_Integer`` (:file:`g-sptain.ads`)
=====================================================

.. index:: GNAT.Spitbol.Table_Integer (g-sptain.ads)

.. index:: Integer maps

.. index:: Maps

.. index:: SPITBOL Tables

A library level of instantiation of ``GNAT.Spitbol.Patterns.Table``
for type ``Standard.Integer``, giving an implementation of maps
from string to integer values.

.. _`GNAT.Spitbol.Table_VString_(g-sptavs.ads)`:

``GNAT.Spitbol.Table_VString`` (:file:`g-sptavs.ads`)
=====================================================

.. index:: GNAT.Spitbol.Table_VString (g-sptavs.ads)

.. index:: String maps

.. index:: Maps

.. index:: SPITBOL Tables

A library level of instantiation of ``GNAT.Spitbol.Patterns.Table`` for
a variable length string type, giving an implementation of general
maps from strings to strings.

.. _`GNAT.SSE_(g-sse.ads)`:

``GNAT.SSE`` (:file:`g-sse.ads`)
================================

.. index:: GNAT.SSE (g-sse.ads)

Root of a set of units aimed at offering Ada bindings to a subset of
the Intel(r) Streaming SIMD Extensions with GNAT on the x86 family of
targets.  It exposes vector component types together with a general
introduction to the binding contents and use.

.. _`GNAT.SSE.Vector_Types_(g-ssvety.ads)`:

``GNAT.SSE.Vector_Types`` (:file:`g-ssvety.ads`)
================================================

.. index:: GNAT.SSE.Vector_Types (g-ssvety.ads)

SSE vector types for use with SSE related intrinsics.

.. _`GNAT.String_Hash(g-strhas.ads)`:

``GNAT.String_Hash`` (:file:`g-strhas.ads`)
===========================================

.. index:: GNAT.String_Hash (g-strhas.ads)

.. index:: Hash functions

Provides a generic hash function working on arrays of scalars. Both the scalar
type and the hash result type are parameters.

.. _`GNAT.Strings_(g-string.ads)`:

``GNAT.Strings`` (:file:`g-string.ads`)
=======================================

.. index:: GNAT.Strings (g-string.ads)

Common String access types and related subprograms. Basically it
defines a string access and an array of string access types.

.. _`GNAT.String_Split_(g-strspl.ads)`:

``GNAT.String_Split`` (:file:`g-strspl.ads`)
============================================

.. index:: GNAT.String_Split (g-strspl.ads)

.. index:: String splitter

Useful string manipulation routines: given a set of separators, split
a string wherever the separators appear, and provide direct access
to the resulting slices. This package is instantiated from
``GNAT.Array_Split``.

.. _`GNAT.Table_(g-table.ads)`:

``GNAT.Table`` (:file:`g-table.ads`)
====================================

.. index:: GNAT.Table (g-table.ads)

.. index:: Table implementation

.. index:: Arrays, extendable

A generic package providing a single dimension array abstraction where the
length of the array can be dynamically modified.

This package provides a facility similar to that of ``GNAT.Dynamic_Tables``,
except that this package declares a single instance of the table type,
while an instantiation of ``GNAT.Dynamic_Tables`` creates a type that can be
used to define dynamic instances of the table.

.. _`GNAT.Task_Lock_(g-tasloc.ads)`:

``GNAT.Task_Lock`` (:file:`g-tasloc.ads`)
=========================================

.. index:: GNAT.Task_Lock (g-tasloc.ads)

.. index:: Task synchronization

.. index:: Task locking

.. index:: Locking

A very simple facility for locking and unlocking sections of code using a
single global task lock.  Appropriate for use in situations where contention
between tasks is very rarely expected.

.. _`GNAT.Time_Stamp_(g-timsta.ads)`:

``GNAT.Time_Stamp`` (:file:`g-timsta.ads`)
==========================================

.. index:: GNAT.Time_Stamp (g-timsta.ads)

.. index:: Time stamp

.. index:: Current time

Provides a simple function that returns a string YYYY-MM-DD HH:MM:SS.SS that
represents the current date and time in ISO 8601 format. This is a very simple
routine with minimal code and there are no dependencies on any other unit.

.. _`GNAT.Threads_(g-thread.ads)`:

``GNAT.Threads`` (:file:`g-thread.ads`)
=======================================

.. index:: GNAT.Threads (g-thread.ads)

.. index:: Foreign threads

.. index:: Threads, foreign

Provides facilities for dealing with foreign threads which need to be known
by the GNAT run-time system. Consult the documentation of this package for
further details if your program has threads that are created by a non-Ada
environment which then accesses Ada code.

.. _`GNAT.Traceback_(g-traceb.ads)`:

``GNAT.Traceback`` (:file:`g-traceb.ads`)
=========================================

.. index:: GNAT.Traceback (g-traceb.ads)

.. index:: Trace back facilities

Provides a facility for obtaining non-symbolic traceback information, useful
in various debugging situations.

.. _`GNAT.Traceback.Symbolic_(g-trasym.ads)`:

``GNAT.Traceback.Symbolic`` (:file:`g-trasym.ads`)
==================================================

.. index:: GNAT.Traceback.Symbolic (g-trasym.ads)

.. index:: Trace back facilities

.. _`GNAT.UTF_32_(g-table.ads)`:

``GNAT.UTF_32`` (:file:`g-table.ads`)
=====================================

.. index:: GNAT.UTF_32 (g-table.ads)

.. index:: Wide character codes

This is a package intended to be used in conjunction with the
``Wide_Character`` type in Ada 95 and the
``Wide_Wide_Character`` type in Ada 2005 (available
in ``GNAT`` in Ada 2005 mode). This package contains
Unicode categorization routines, as well as lexical
categorization routines corresponding to the Ada 2005
lexical rules for identifiers and strings, and also a
lower case to upper case fold routine corresponding to
the Ada 2005 rules for identifier equivalence.

.. _`GNAT.Wide_Spelling_Checker_(g-u3spch.ads)`:

``GNAT.Wide_Spelling_Checker`` (:file:`g-u3spch.ads`)
=====================================================

.. index:: GNAT.Wide_Spelling_Checker (g-u3spch.ads)

.. index:: Spell checking

Provides a function for determining whether one wide wide string is a plausible
near misspelling of another wide wide string, where the strings are represented
using the UTF_32_String type defined in System.Wch_Cnv.

.. _`GNAT.Wide_Spelling_Checker_(g-wispch.ads)`:

``GNAT.Wide_Spelling_Checker`` (:file:`g-wispch.ads`)
=====================================================

.. index:: GNAT.Wide_Spelling_Checker (g-wispch.ads)

.. index:: Spell checking

Provides a function for determining whether one wide string is a plausible
near misspelling of another wide string.

.. _`GNAT.Wide_String_Split_(g-wistsp.ads)`:

``GNAT.Wide_String_Split`` (:file:`g-wistsp.ads`)
=================================================

.. index:: GNAT.Wide_String_Split (g-wistsp.ads)

.. index:: Wide_String splitter

Useful wide string manipulation routines: given a set of separators, split
a wide string wherever the separators appear, and provide direct access
to the resulting slices. This package is instantiated from
``GNAT.Array_Split``.

.. _`GNAT.Wide_Wide_Spelling_Checker_(g-zspche.ads)`:

``GNAT.Wide_Wide_Spelling_Checker`` (:file:`g-zspche.ads`)
==========================================================

.. index:: GNAT.Wide_Wide_Spelling_Checker (g-zspche.ads)

.. index:: Spell checking

Provides a function for determining whether one wide wide string is a plausible
near misspelling of another wide wide string.

.. _`GNAT.Wide_Wide_String_Split_(g-zistsp.ads)`:

``GNAT.Wide_Wide_String_Split`` (:file:`g-zistsp.ads`)
======================================================

.. index:: GNAT.Wide_Wide_String_Split (g-zistsp.ads)

.. index:: Wide_Wide_String splitter

Useful wide wide string manipulation routines: given a set of separators, split
a wide wide string wherever the separators appear, and provide direct access
to the resulting slices. This package is instantiated from
``GNAT.Array_Split``.

.. _`Interfaces.C.Extensions_(i-cexten.ads)`:

``Interfaces.C.Extensions`` (:file:`i-cexten.ads`)
==================================================

.. index:: Interfaces.C.Extensions (i-cexten.ads)

This package contains additional C-related definitions, intended
for use with either manually or automatically generated bindings
to C libraries.

.. _`Interfaces.C.Streams_(i-cstrea.ads)`:

``Interfaces.C.Streams`` (:file:`i-cstrea.ads`)
===============================================

.. index:: Interfaces.C.Streams (i-cstrea.ads)

.. index::  C streams, interfacing

This package is a binding for the most commonly used operations
on C streams.

.. _`Interfaces.Packed_Decimal_(i-pacdec.ads)`:

``Interfaces.Packed_Decimal`` (:file:`i-pacdec.ads`)
====================================================

.. index:: Interfaces.Packed_Decimal (i-pacdec.ads)

.. index::  IBM Packed Format

.. index::  Packed Decimal

This package provides a set of routines for conversions to and
from a packed decimal format compatible with that used on IBM
mainframes.

.. _`Interfaces.VxWorks_(i-vxwork.ads)`:

``Interfaces.VxWorks`` (:file:`i-vxwork.ads`)
=============================================

.. index:: Interfaces.VxWorks (i-vxwork.ads)

.. index:: Interfacing to VxWorks

.. index:: VxWorks, interfacing

This package provides a limited binding to the VxWorks API.
In particular, it interfaces with the
VxWorks hardware interrupt facilities.

.. _`Interfaces.VxWorks.Int_Connection_(i-vxinco.ads)`:

``Interfaces.VxWorks.Int_Connection`` (:file:`i-vxinco.ads`)
============================================================

.. index:: Interfaces.VxWorks.Int_Connection (i-vxinco.ads)

.. index:: Interfacing to VxWorks

.. index:: VxWorks, interfacing

This package provides a way for users to replace the use of
intConnect() with a custom routine for installing interrupt
handlers.

.. _`Interfaces.VxWorks.IO_(i-vxwoio.ads)`:

``Interfaces.VxWorks.IO`` (:file:`i-vxwoio.ads`)
================================================

.. index:: Interfaces.VxWorks.IO (i-vxwoio.ads)

.. index:: Interfacing to VxWorks' I/O

.. index:: VxWorks, I/O interfacing

.. index:: VxWorks, Get_Immediate

.. index:: Get_Immediate, VxWorks

This package provides a binding to the ioctl (IO/Control)
function of VxWorks, defining a set of option values and
function codes. A particular use of this package is
to enable the use of Get_Immediate under VxWorks.

.. _`System.Address_Image_(s-addima.ads)`:

``System.Address_Image`` (:file:`s-addima.ads`)
===============================================

.. index:: System.Address_Image (s-addima.ads)

.. index:: Address image

.. index:: Image, of an address

This function provides a useful debugging
function that gives an (implementation dependent)
string which identifies an address.

.. _`System.Assertions_(s-assert.ads)`:

``System.Assertions`` (:file:`s-assert.ads`)
============================================

.. index:: System.Assertions (s-assert.ads)

.. index:: Assertions

.. index:: Assert_Failure, exception

This package provides the declaration of the exception raised
by an run-time assertion failure, as well as the routine that
is used internally to raise this assertion.

.. _`System.Atomic_Counters_(s-atocou.ads)`:

``System.Atomic_Counters`` (:file:`s-atocou.ads`)
=================================================

.. index:: System.Atomic_Counters (s-atocou.ads)

This package provides the declaration of an atomic counter type,
together with efficient routines (using hardware
synchronization primitives) for incrementing, decrementing,
and testing of these counters. This package is implemented
on most targets, including all Alpha, ia64, PowerPC, SPARC V9,
x86, and x86_64 platforms.

.. _`System.Memory_(s-memory.ads)`:

``System.Memory`` (:file:`s-memory.ads`)
========================================

.. index:: System.Memory (s-memory.ads)

.. index:: Memory allocation

This package provides the interface to the low level routines used
by the generated code for allocation and freeing storage for the
default storage pool (analogous to the C routines malloc and free.
It also provides a reallocation interface analogous to the C routine
realloc. The body of this unit may be modified to provide alternative
allocation mechanisms for the default pool, and in addition, direct
calls to this unit may be made for low level allocation uses (for
example see the body of ``GNAT.Tables``).

.. _`System.Multiprocessors_(s-multip.ads)`:

``System.Multiprocessors`` (:file:`s-multip.ads`)
=================================================

.. index:: System.Multiprocessors (s-multip.ads)

.. index:: Multiprocessor interface

This is an Ada 2012 unit defined in the Ada 2012 Reference Manual, but
in GNAT we also make it available in Ada 95 and Ada 2005 (where it is
technically an implementation-defined addition).

.. _`System.Multiprocessors.Dispatching_Domains_(s-mudido.ads)`:

``System.Multiprocessors.Dispatching_Domains`` (:file:`s-mudido.ads`)
=====================================================================

.. index:: System.Multiprocessors.Dispatching_Domains (s-mudido.ads)

.. index:: Multiprocessor interface

This is an Ada 2012 unit defined in the Ada 2012 Reference Manual, but
in GNAT we also make it available in Ada 95 and Ada 2005 (where it is
technically an implementation-defined addition).

.. _`System.Partition_Interface_(s-parint.ads)`:

``System.Partition_Interface`` (:file:`s-parint.ads`)
=====================================================

.. index:: System.Partition_Interface (s-parint.ads)

.. index:: Partition interfacing functions

This package provides facilities for partition interfacing.  It
is used primarily in a distribution context when using Annex E
with ``GLADE``.

.. _`System.Pool_Global_(s-pooglo.ads)`:

``System.Pool_Global`` (:file:`s-pooglo.ads`)
=============================================

.. index:: System.Pool_Global (s-pooglo.ads)

.. index:: Storage pool, global

.. index:: Global storage pool

This package provides a storage pool that is equivalent to the default
storage pool used for access types for which no pool is specifically
declared. It uses malloc/free to allocate/free and does not attempt to
do any automatic reclamation.

.. _`System.Pool_Local_(s-pooloc.ads)`:

``System.Pool_Local`` (:file:`s-pooloc.ads`)
============================================

.. index:: System.Pool_Local (s-pooloc.ads)

.. index:: Storage pool, local

.. index:: Local storage pool

This package provides a storage pool that is intended for use with locally
defined access types. It uses malloc/free for allocate/free, and maintains
a list of allocated blocks, so that all storage allocated for the pool can
be freed automatically when the pool is finalized.

.. _`System.Restrictions_(s-restri.ads)`:

``System.Restrictions`` (:file:`s-restri.ads`)
==============================================

.. index:: System.Restrictions (s-restri.ads)

.. index:: Run-time restrictions access

This package provides facilities for accessing at run time
the status of restrictions specified at compile time for
the partition. Information is available both with regard
to actual restrictions specified, and with regard to
compiler determined information on which restrictions
are violated by one or more packages in the partition.

.. _`System.Rident_(s-rident.ads)`:

``System.Rident`` (:file:`s-rident.ads`)
========================================

.. index:: System.Rident (s-rident.ads)

.. index:: Restrictions definitions

This package provides definitions of the restrictions
identifiers supported by GNAT, and also the format of
the restrictions provided in package System.Restrictions.
It is not normally necessary to ``with`` this generic package
since the necessary instantiation is included in
package System.Restrictions.

.. _`System.Strings.Stream_Ops_(s-ststop.ads)`:

``System.Strings.Stream_Ops`` (:file:`s-ststop.ads`)
====================================================

.. index:: System.Strings.Stream_Ops (s-ststop.ads)

.. index:: Stream operations

.. index:: String stream operations

This package provides a set of stream subprograms for standard string types.
It is intended primarily to support implicit use of such subprograms when
stream attributes are applied to string types, but the subprograms in this
package can be used directly by application programs.

.. _`System.Unsigned_Types_(s-unstyp.ads)`:

``System.Unsigned_Types`` (:file:`s-unstyp.ads`)
================================================

.. index:: System.Unsigned_Types (s-unstyp.ads)

This package contains definitions of standard unsigned types that
correspond in size to the standard signed types declared in Standard,
and (unlike the types in Interfaces) have corresponding names. It
also contains some related definitions for other specialized types
used by the compiler in connection with packed array types.

.. _`System.Wch_Cnv_(s-wchcnv.ads)`:

``System.Wch_Cnv`` (:file:`s-wchcnv.ads`)
=========================================

.. index:: System.Wch_Cnv (s-wchcnv.ads)

.. index:: Wide Character, Representation

.. index:: Wide String, Conversion

.. index:: Representation of wide characters

This package provides routines for converting between
wide and wide wide characters and a representation as a value of type
``Standard.String``, using a specified wide character
encoding method.  It uses definitions in
package ``System.Wch_Con``.

.. _`System.Wch_Con_(s-wchcon.ads)`:

``System.Wch_Con`` (:file:`s-wchcon.ads`)
=========================================

.. index:: System.Wch_Con (s-wchcon.ads)

This package provides definitions and descriptions of
the various methods used for encoding wide characters
in ordinary strings.  These definitions are used by
the package ``System.Wch_Cnv``.
