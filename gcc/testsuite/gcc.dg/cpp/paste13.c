/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* This used to be recognized as a comment when lexing after pasting
   spellings.  Neil Booth, 9 Oct 2002.  */

#define a /##/=
a			/* { dg-warning "valid preprocessing tok" } */
