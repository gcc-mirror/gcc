/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-O3 -march=zEC12 -mno-htm -fno-ipa-icf" } */

#pragma GCC target("htm")
void p1(void)
{
#ifndef __HTM__
#error __HTM__ is not defined
#endif
  __builtin_tend ();
}
#pragma GCC reset_options

#pragma GCC target("no-htm")
void p0(void)
{
#ifdef __HTM__
#error __HTM__ is defined
#endif
  __builtin_tend (); /* { dg-error "is not supported without '-mhtm'" } */
}
#pragma GCC reset_options

__attribute__ ((target("htm")))
void a1(void)
{
#ifdef __HTM__
#error __HTM__ is defined
#endif
  __builtin_tend ();
}

__attribute__ ((target("no-htm")))
void a0(void)
{
#ifdef __HTM__
#error __HTM__ is defined
#endif
  __builtin_tend (); /* { dg-error "is not supported without '-mhtm'" } */
}

void htmd(void)
{
#ifdef __HTM__
#error __HTM__ is defined
#endif
  __builtin_tend (); /* { dg-error "is not supported without '-mhtm'" } */
}
