/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-fpreprocessed" } */

/* Source: Neil Booth, 16 Sep 2001.

   This file, with an indented line marker, is not possible without
   user editing of preprocessed output, or the user using
   -fpreprocessed on raw source.  Nevertheless, we should not
   segfault.  This is a test that we don't back up two tokens in
   cpplib.c - one to back up over the number, and one when we
   recognise that it's not a valid directive in preprocessed input.  */

 # 1 "foo.c"
