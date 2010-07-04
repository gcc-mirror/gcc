// PR c++/23337; caused an ICE in component_ref_field_offset
// { dg-options "" }
// { dg-options "-mmmx" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
// { dg-prune-output "mangled name" }
typedef int vec __attribute__ ((vector_size (8)));
extern int bar (vec);
int
foo (int i)
{
  vec a[] = { (vec) { 0, i }, (vec) { 4, 5 } };
  return bar (a[0]) + bar (a[1]);
}
