/* Copyright (C) 2000, 2003 Free Software Foundation, Inc.  */
/* { dg-do preprocess } */
/* { dg-options "-C" } */
/* Test that multi-line tokens are recognized by cpp0 as being
   multiline .  Source: Neil Booth, 17 Dec 2000.  */
/* Line 1
   Line 2
   Line 3
   Line 4
*/
"line 1
 line 2
 line 3
 line 4"
L"line 1
  line 2
  line 3
  line 4"
/* Nowhere in the output of this file should there be a blank line.
   We check for that in the .i file.
   { dg-final { scan-file-not multiline.i "(^|\\n)\\n" } } */
/* { dg-bogus "missing term" "multiline strings" { target *-*-* } 11 } */
/* { dg-bogus "warning" "warning in place of error" { target *-*-* } 15 } */
