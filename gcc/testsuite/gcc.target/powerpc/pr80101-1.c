/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-require-effective-target dfp_hw } */
/* { dg-options "-mcpu=power6 -mno-sched-epilog -Ofast" } */

/* Prior to resolving PR 80101, this test case resulted in an internal
   compiler error.  The role of this test program is to assure that
   dejagnu's "test for excess errors" does not find any.  */

int b;

void e ();

int c ()
{
  struct
  {
    int a[b];
  } d;
  if (d.a[0])
    e ();
}
