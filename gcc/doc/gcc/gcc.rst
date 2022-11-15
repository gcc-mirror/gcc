..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. %**start of header

.. INTERNALS is used by md.texi to determine whether to include the
   whole of that file, in the internals manual, or only the part
   dealing with constraints, in the user manual.

.. NOTE: checks/things to do:
    c
   -have bob do a search in all seven files for "mew" (ideally -mew,
    but i may have forgotten the occasional "-"..).
       Just checked... all have `-'!  Bob 22Jul96
       Use this to search:   grep -n '\-\-mew' *.texi
   -item/itemx, text after all (sub/sub)section titles, etc..
   -consider putting the lists of options on pp 17-> etc in columns or
    some such.
   -overfulls.  do a search for "mew" in the files, and you will see
     overfulls that i noted but could not deal with.
   -have to add text:  beginning of chapter 8
    c
   anything else?                       -mew 10feb93

.. Create a separate index for command line options.

.. Merge the standard indexes into a single one.

.. %**end of header

.. index:: introduction

.. _top:

Introduction
============

This manual documents how to use the GNU compilers,
as well as their features and incompatibilities, and how to report
bugs.  It corresponds to the compilers
|package_version|
version |gcc_version|.
The internals of the GNU compilers, including how to port them to new
targets and some information about how to write front ends for new
languages, are documented in a separate manual.  See :ref:`gccint:top`.
