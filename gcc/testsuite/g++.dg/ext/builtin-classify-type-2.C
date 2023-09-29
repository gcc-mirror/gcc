// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo (int n)
{
  __builtin_classify_type (enum E { E1, E2 });	// { dg-error "types may not be defined in '__builtin_classify_type' calls" }
  __builtin_classify_type (struct S { int s; });// { dg-error "types may not be defined in '__builtin_classify_type' calls" }
  __builtin_classify_type (union U { int u; });	// { dg-error "types may not be defined in '__builtin_classify_type' calls" }
  __builtin_classify_type (int [2 * n + 36]);
}
