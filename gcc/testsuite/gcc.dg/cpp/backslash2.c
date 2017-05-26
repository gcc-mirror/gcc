/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test warnings for backslash-space-newline.
   Source: Neil Booth. 6 Dec 2000.  */

foo \  
bar
/* { dg-warning "separated by space" "" { target *-*-* } .-2 } */

/* foo \   
   bar */
/* { dg-bogus "separated by space" "" { target *-*-* } .-2 } */
