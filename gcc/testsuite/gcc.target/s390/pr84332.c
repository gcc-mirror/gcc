/* { dg-do compile } */
/* { dg-options "-march=z900 -fstack-clash-protection --param stack-clash-protection-probe-interval=16" } */

struct b
{
  char a[65536];
};

void c (void) { struct b d; }
