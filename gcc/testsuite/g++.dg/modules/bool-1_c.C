// { dg-additional-options {-fmodules-ts -Wno-psabi} }

import "bool-1_b.H";

void frob (signed char __attribute__ ((__vector_size__ (16))) arg)
{
  _mm_cmplt_epi8 (arg, arg);
}
