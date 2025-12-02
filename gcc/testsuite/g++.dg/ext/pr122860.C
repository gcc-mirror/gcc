// PR c++/122860
// { dg-do compile }
// { dg-options "-Wno-register" }

void
foo ()
{
  register __UINTPTR_TYPE__ val asm ("1") = (__UINTPTR_TYPE__) &val;	// { dg-error "address of explicit register variable 'val' requested" }
}

template <typename T>
void
bar ()
{
  register T val asm ("1") = (T) &val;	// { dg-error "address of explicit register variable 'val' requested" }
}

template <typename T>
void
baz ()
{
  register __UINTPTR_TYPE__ val asm ("1") = (__UINTPTR_TYPE__) &val;	// { dg-error "address of explicit register variable 'val' requested" }
}

void
qux ()
{
  bar <__UINTPTR_TYPE__> ();
  baz <int> ();
}
