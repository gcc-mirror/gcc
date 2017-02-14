/* Copyright (c) 2002 Free Software Foundation Inc.  */

/* Test that macros are not expanded in the <> quotes of #inlcude.  */

#define builtins 1
#include <builtins.h>		/* { dg-bogus "o such file or directory" } */
