/* PR target/70062 */
/* { dg-options "-minline-all-stringops -minline-stringops-dynamically -mmemcpy-strategy=libcall:-1:noalign -Wno-psabi" } */
/* { dg-additional-options "-mtune=k6-2" { target ia32 } } */

typedef int V __attribute__ ((vector_size (32)));

V
foo (V x)
{
  return (V) { x[0] };
}
