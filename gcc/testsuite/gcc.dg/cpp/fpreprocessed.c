/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options -fpreprocessed } */

/* Source: Jason Merrill, 19 Nov 2001.  We'd try and back up a token
   and move to a non-existent token run with -fpreprocessed on a file
   without a leading # line.  */

foo
