..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _alternatives-to-wrapper-ifndef:

Alternatives to Wrapper #ifndef
*******************************

CPP supports two more ways of indicating that a header file should be
read only once.  Neither one is as portable as a wrapper :samp:`#ifndef`
and we recommend you do not use them in new programs, with the caveat
that :samp:`#import` is standard practice in Objective-C.

.. index:: #import

CPP supports a variant of :samp:`#include` called :samp:`#import` which
includes a file, but does so at most once.  If you use :samp:`#import`
instead of :samp:`#include`, then you don't need the conditionals
inside the header file to prevent multiple inclusion of the contents.
:samp:`#import` is standard in Objective-C, but is considered a
deprecated extension in C and C++.

:samp:`#import` is not a well designed feature.  It requires the users of
a header file to know that it should only be included once.  It is much
better for the header file's implementor to write the file so that users
don't need to know this.  Using a wrapper :samp:`#ifndef` accomplishes
this goal.

In the present implementation, a single use of :samp:`#import` will
prevent the file from ever being read again, by either :samp:`#import` or
:samp:`#include`.  You should not rely on this; do not use both
:samp:`#import` and :samp:`#include` to refer to the same header file.

Another way to prevent a header file from being included more than once
is with the :samp:`#pragma once` directive (see :ref:`pragmas`).
:samp:`#pragma once` does not have the problems that :samp:`#import` does,
but it is not recognized by all preprocessors, so you cannot rely on it
in a portable program.