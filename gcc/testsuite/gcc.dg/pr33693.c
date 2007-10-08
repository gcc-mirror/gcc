/* { dg-do compile } */

/* This used to ICE with type-checking enabled.  */

unsigned long modify_field (unsigned long mask, long fieldval)
{
  return (~fieldval & ~mask);
}
