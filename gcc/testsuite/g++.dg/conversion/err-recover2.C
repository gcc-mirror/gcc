// PR c++/94475
// { dg-do compile }

unsigned char
sr ()
{
  const unsigned char xz = EI; // { dg-error "not declared" }

  return xz - (xz >> 1);
}
