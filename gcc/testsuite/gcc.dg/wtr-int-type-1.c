/* Test for -Wtraditional warnings on integer constant types.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc (void)
{
  long long i;
  
  /* Octal and hex values shouldn't issue -Wtraditional warnings. */
  i = 0x80000000;
  i = 0xFFFFFFFF;
  i = 037777777777;

  i = 0x8000000000000000;
  i = 0xFFFFFFFFFFFFFFFF;
  i = 01777777777777777777777;

  /* We expect to get either a "width of integer constant changes with
     -traditional" warning or an "integer constant is unsigned in ISO
     C, signed with -traditional" warning depending on whether we are
     testing on a 32 or 64 bit platform.  Either warning means the
     test passes and both matched by checking for "integer constant".  */
  i = 18446744073709551615; /* { dg-warning "integer constant" "integer constant" } */
  
# 29 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  i = 0x80000000;
  i = 0xFFFFFFFF;
  i = 037777777777;
  
  i = 0x8000000000000000;
  i = 0xFFFFFFFFFFFFFFFF;
  i = 01777777777777777777777;
  
  i = 9223372036854775807;
  i = 18446744073709551615;
}

/* Ignore "decimal constant is so large that it is unsigned" warnings.  */
/* { dg-warning "decimal constant" "decimal constant" { target *-*-* } 26 } */
