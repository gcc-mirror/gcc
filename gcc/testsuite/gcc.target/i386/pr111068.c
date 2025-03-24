/* PR target/111068 */
/* { dg-do compile } */
/* { dg-options "-ffloat-store -mavx10.1" } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

typedef _Float16 __attribute__((__vector_size__ (8))) V;
V u, v, w;

void
foo (void)
{
  v /= __builtin_shufflevector (w, u, 3, 3, 6, 1);
}
