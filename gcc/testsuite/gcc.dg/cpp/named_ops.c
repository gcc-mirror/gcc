/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options -fpreprocessed } */

/* Tests that C++ named ops are still there with -fpreprocessed.  */

/* Source: Neil Booth, 23 May 2002.  */

#if 2 xor 2
#endif
