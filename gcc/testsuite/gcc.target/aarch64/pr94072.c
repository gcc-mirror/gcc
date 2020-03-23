/* { dg-options "-msve-vector-bits=512" } */

#pragma GCC target "+nosve"

void
foo (void)
{
  (int __attribute__ ((__vector_size__ (64)))){};
}
