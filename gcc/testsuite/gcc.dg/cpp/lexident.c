/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-trigraphs -fdollars-in-identifiers" } */

/* Test lexing of identifiers.  */

/* Escaped newlines, _ and $ in identifiers.  */
#def\
\
ine foo_

#d\
ef??/
in\
e b\
a$r

#ifndef foo_
#error foo_
#endif

#ifndef ba$r
#error ba$r
#endif
