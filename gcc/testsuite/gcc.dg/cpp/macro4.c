/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test source Neil Booth.  */

#define glue(x, y) x ## y
#define xglue(x, y) glue (x, y)

/* Should expand to glue (1, 2), then 12.  */
#if glue (xgl, ue) (1, 2) != 12
#error glue macro
#endif
