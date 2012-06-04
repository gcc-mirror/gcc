/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/*
  { dg-options "-ftrack-macro-expansion=0" }
  { dg-do preprocess }
*/

/* This used to be recognized as a comment when lexing after pasting
   spellings.  Neil Booth, 9 Oct 2002.  */

#define a /##/=
a			/* { dg-error "valid preprocessing tok" } */
