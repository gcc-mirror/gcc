/* { dg-do compile } */
/* { dg-options "-O2" } */

static inline unsigned
parity (unsigned x)
{
  return (unsigned) __builtin_parity (x);
}

unsigned
f (unsigned rpoly)
{
  return parity (rpoly & 1) ^ parity (rpoly & 6);
}
