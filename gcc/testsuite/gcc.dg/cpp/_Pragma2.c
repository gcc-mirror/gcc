/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Don't allow directives in a _Pragma string. */

/* Contributed by Neil Booth 14 Nov 2000.  */

_Pragma("#define test")

#ifdef test
#error Do not allow directives in _Pragma strings
#endif
