/* Copyright (C) 2000 Free Software Foundation, Inc.  */
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
   { dg-final { if ![file exists multiline.i] { return }                } }
   { dg-final { if \{ [grep multiline.i "^$"] == "" \} \{               } }
   { dg-final { return \}                                               } }
   { dg-final { fail "multiline.c: multi-line tokens"                   } } */
/* { dg-error "missing term" "multiline strings" { target *-*-* } 11 } */
/* { dg-error "missing term" "multiline strings" { target *-*-* } 14 } */
/* { dg-error "missing term" "multiline strings" { target *-*-* } 15 } */
/* { dg-error "missing term" "multiline strings" { target *-*-* } 18 } */
/* { dg-bogus "warning" "warning in place of error" { target *-*-* } 11 } */
/* { dg-bogus "warning" "warning in place of error" { target *-*-* } 14 } */
/* { dg-bogus "warning" "warning in place of error" { target *-*-* } 15 } */
/* { dg-bogus "warning" "warning in place of error" { target *-*-* } 18 } */
