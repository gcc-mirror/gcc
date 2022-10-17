.. _About_This_Guide:

****************
About This Guide
****************

.. only:: PRO

   For ease of exposition, 'GNAT Pro' will be referred to simply as
   'GNAT' in the remainder of this document.

This manual contains useful information in writing programs using the
GNAT compiler.  It includes information on implementation dependent
characteristics of GNAT, including all the information required by
Annex M of the Ada language standard.

GNAT implements Ada 95, Ada 2005 and Ada 2012, and it may also be
invoked in Ada 83 compatibility mode.
By default, GNAT assumes Ada 2012,
but you can override with a compiler switch
to explicitly specify the language version.
(Please refer to the *GNAT User's Guide* for details on these switches.)
Throughout this manual, references to 'Ada' without a year suffix
apply to all the Ada versions of the language.

Ada is designed to be highly portable.
In general, a program will have the same effect even when compiled by
different compilers on different platforms.
However, since Ada is designed to be used in a
wide variety of applications, it also contains a number of system
dependent features to be used in interfacing to the external world.

.. index:: Implementation-dependent features

.. index:: Portability

Note: Any program that makes use of implementation-dependent features
may be non-portable.  You should follow good programming practice and
isolate and clearly document any sections of your program that make use
of these features in a non-portable manner.

What This Reference Manual Contains
===================================

This reference manual contains the following chapters:

* :ref:`Implementation_Defined_Pragmas`, lists GNAT implementation-dependent
  pragmas, which can be used to extend and enhance the functionality of the
  compiler.

* :ref:`Implementation_Defined_Attributes`, lists GNAT
  implementation-dependent attributes, which can be used to extend and
  enhance the functionality of the compiler.

* :ref:`Standard_and_Implementation_Defined_Restrictions`, lists GNAT
  implementation-dependent restrictions, which can be used to extend and
  enhance the functionality of the compiler.

* :ref:`Implementation_Advice`, provides information on generally
  desirable behavior which are not requirements that all compilers must
  follow since it cannot be provided on all systems, or which may be
  undesirable on some systems.

* :ref:`Implementation_Defined_Characteristics`, provides a guide to
  minimizing implementation dependent features.

* :ref:`Intrinsic_Subprograms`, describes the intrinsic subprograms
  implemented by GNAT, and how they can be imported into user
  application programs.

* :ref:`Representation_Clauses_and_Pragmas`, describes in detail the
  way that GNAT represents data, and in particular the exact set
  of representation clauses and pragmas that is accepted.

* :ref:`Standard_Library_Routines`, provides a listing of packages and a
  brief description of the functionality that is provided by Ada's
  extensive set of standard library routines as implemented by GNAT.

* :ref:`The_Implementation_of_Standard_I/O`, details how the GNAT
  implementation of the input-output facilities.

* :ref:`The_GNAT_Library`, is a catalog of packages that complement
  the Ada predefined library.

* :ref:`Interfacing_to_Other_Languages`, describes how programs
  written in Ada using GNAT can be interfaced to other programming
  languages.

* :ref:`Specialized_Needs_Annexes`, describes the GNAT implementation of all
  of the specialized needs annexes.

* :ref:`Implementation_of_Specific_Ada_Features`, discusses issues related
  to GNAT's implementation of machine code insertions, tasking, and several
  other features.

* :ref:`Implementation_of_Ada_2012_Features`, describes the status of the
  GNAT implementation of the Ada 2012 language standard.

* :ref:`Security_Hardening_Features` documents GNAT extensions aimed
  at security hardening.

* :ref:`Obsolescent_Features` documents implementation dependent features,
  including pragmas and attributes, which are considered obsolescent, since
  there are other preferred ways of achieving the same results. These
  obsolescent forms are retained for backwards compatibility.

* :ref:`Compatibility_and_Porting_Guide` presents some guidelines for
  developing portable Ada code, describes the compatibility issues that
  may arise between GNAT and other Ada compilation systems (including those
  for Ada 83), and shows how GNAT can expedite porting applications
  developed in other Ada environments.

* :ref:`gnu_fdl` contains the license for this document.

.. index:: Ada 95 Language Reference Manual

.. index:: Ada 2005 Language Reference Manual

This reference manual assumes a basic familiarity with the Ada 95 language, as
described in the
:title:`International Standard ANSI/ISO/IEC-8652:1995`.
It does not require knowledge of the new features introduced by Ada 2005 or
Ada 2012.
All three reference manuals are included in the GNAT documentation
package.

Conventions
===========
.. index:: Conventions, typographical

.. index:: Typographical conventions

Following are examples of the typographical and graphic conventions used
in this guide:

* ``Functions``, ``utility program names``, ``standard names``,
  and ``classes``.

* ``Option flags``

* :file:`File names`

* ``Variables``

* *Emphasis*

* [optional information or parameters]

* Examples are described by text

  ::

    and then shown this way.

* Commands that are entered by the user are shown as preceded by a prompt string
  comprising the ``$`` character followed by a space.

Related Information
===================

See the following documents for further information on GNAT:

* :title:`GNAT User's Guide for Native Platforms`,
  which provides information on how to use the
  GNAT development environment.

* :title:`Ada 95 Reference Manual`, the Ada 95 programming language standard.

* :title:`Ada 95 Annotated Reference Manual`, which is an annotated version
  of the Ada 95 standard.  The annotations describe
  detailed aspects of the design decision, and in particular contain useful
  sections on Ada 83 compatibility.

* :title:`Ada 2005 Reference Manual`, the Ada 2005 programming language standard.

* :title:`Ada 2005 Annotated Reference Manual`, which is an annotated version
  of the Ada 2005 standard.  The annotations describe
  detailed aspects of the design decision.

* :title:`Ada 2012 Reference Manual`, the Ada 2012 programming language standard.

* :title:`DEC Ada, Technical Overview and Comparison on DIGITAL Platforms`,
  which contains specific information on compatibility between GNAT and
  DEC Ada 83 systems.

* :title:`DEC Ada, Language Reference Manual`, part number AA-PYZAB-TK, which
  describes in detail the pragmas and attributes provided by the DEC Ada 83
  compiler system.
