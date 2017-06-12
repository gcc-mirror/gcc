/* Test for -Wtraditional warnings on integer constant types.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -Wtraditional" } */

void
testfunc ()
{
  long long i;
  
  /* Octal and hex values shouldn't issue -Wtraditional warnings. */
  i = 0x80000000;
  i = 0xFFFFFFFF;
  i = 037777777777;

  i = 0x8000000000000000;
  i = 0xFFFFFFFFFFFFFFFF;
  i = 01777777777777777777777;

  /* Nor should values outside the range of (32-bit) unsigned long but
     inside the range of long long.  [since -traditional has no long long,
     we can pretend it worked the way it does in C99.]  */
  i = 9223372036854775807;

  /* But this one should, since it doesn't fit in long (long), but
     does fit in unsigned long (long).  */
  i = 18446744073709551615; /* { dg-warning "integer constant is so large that it is unsigned" "so large" } */
  /* { dg-warning "this decimal constant would be unsigned in ISO C90" "ISO C90" { target *-*-* } .-1 } */

# 29 "sys-header.h" 3
}

void
testfunc2( ) 
{ 
  long long i;

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

