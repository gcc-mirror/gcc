/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-funsigned-char -fsigned-char -traditional-cpp" } */

#if defined (__CHAR_UNSIGNED__)
# error __CHAR_UNSIGNED__ defined
#endif
