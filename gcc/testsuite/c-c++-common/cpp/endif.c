/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test case for PR preprocessor/6386 by Andreas Schwab.  We'd back up
   over the CPP_EOF token (indicating not a funlike macro invocation)
   in the header file, which would then be passed through as a real
   EOF, leading to an early exit (and therefore bogus complaint about
   unterminated #if).  */

#define S(x)
#if 1
#include "endif.h"
#endif
