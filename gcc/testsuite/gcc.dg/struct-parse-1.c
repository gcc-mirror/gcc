/* Copyright (C) 2006 Free Software Foundation, Inc. */
/* Contributed by Carlos O'Donell on 2006-03-31 */

/* This code caused the C frontend to loop 
   forever exhausting all system memory, or ICE */
/* Origin: Carlos O'Donell <carlos@codesourcery.com> */

/* { dg-options "-std=c99" } */
struct s { int a; int b; struct t c; }; /* { dg-error "field 'c' has incomplete type" } */
struct s d = { .b = 0 };

