/* Simplified from testcase by David Staepelaere <staapa@ultimatech.com> */

/* { dg-do compile } */
/* { dg-options -mcpu=ultrasparc } */

int foo(long long y) {
  return -1 * y;
}
