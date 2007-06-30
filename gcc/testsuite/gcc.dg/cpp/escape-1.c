/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* This tests various diagnostics about escape sequences, for both
   the preprocessor and the compiler.

   Neil Booth, 22 May 2001.  */

#if '\x'	/* { dg-error "no following" "\x with no digits" } */
#endif
#if '\x400'	/* { dg-error "out of range" "\x out of range" } */
#endif
#if '\x0ff'	/* { dg-bogus "out of range" "\x out of range" } */
#endif
#if '\400'	/* { dg-error "out of range" "\x out of range" } */
#endif
#if '\377'	/* { dg-bogus "out of range" "bogus \x out of range" } */
#endif
#if '\177' != 0x7f /* { dg-bogus "out of range" "bogus \x out of range" } */
#error bad octal /* { dg-bogus "bad" "bad octal evaluation" } */
#endif
#if '\0377'	/* { dg-warning "multi" "too long octal" } */
#endif
#if '\p'	/* { dg-error "unknown escape" "unknown escape seq" } */
#endif

void foo ()
{
  int c;

  c = '\x';	/* { dg-error "no following" "\x with no digits" } */
  c = '\x100';	/* { dg-error "out of range" "\x out of range" } */
  c = '\x0ff';	/* { dg-bogus "out of range" "\x out of range" } */
  c = '\400';	/* { dg-error "out of range" "\x out of range" } */
  c = '\377';	/* { dg-bogus "out of range" "bogus \x out of range" } */
  c = '\0377';	/* { dg-warning "multi" "too long octal" } */
  c = '\p';	/* { dg-error "unknown escape" "unknown escape seq" } */
}
