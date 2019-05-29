/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-Wunknown-pragmas" } */

/* We used to get "unspellable token: CPP_EOF" warnings.  */

#pragma				/* { dg-warning "-:ignoring '#pragma" } */
#pragma ~			/* { dg-warning "-:ignoring '#pragma" } */
#pragma baz			/* { dg-warning "-:ignoring '#pragma" } */
#pragma baz baz			/* { dg-warning "-:ignoring '#pragma baz baz'" } */
