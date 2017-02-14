// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-error=pedantic" }

#define INTB (__SIZEOF_INT__ * __CHAR_BIT__)
struct S { int i : INTB * 3 / 4; S (); };
struct T : public S { int j : INTB / 4; T (); };
struct U { int i : INTB * 3 / 4; int j : INTB / 4; };
struct V { int i : INTB * 3 / 4; int j : INTB / 4 + 1; };
struct W {};
struct X : public W { int i; void bar (); };
struct Y {
  char a[3]; char b[];   // { dg-warning "forbids flexible array member" }
};
struct Z { int a; float b; };
struct A { int i : INTB * 2; int j; };			// { dg-warning "exceeds its type" }
union B { long a; unsigned long b; };
union C { int a; int b : INTB - 1; };
struct D { int a : INTB + 1; int b : INTB - 1; };	// { dg-warning "exceeds its type" }
static_assert (__has_unique_object_representations (char) == true, "");
static_assert (__has_unique_object_representations (unsigned char) == true, "");
static_assert (__has_unique_object_representations (int) == true, "");
static_assert (__has_unique_object_representations (unsigned int) == true, "");
static_assert (__has_unique_object_representations (bool) == true, "");
static_assert (sizeof (S) != sizeof (int) || __has_unique_object_representations (S) == false, "");
static_assert (sizeof (T) != sizeof (int) || __has_unique_object_representations (T) == true, "");
static_assert (sizeof (U) != sizeof (int) || __has_unique_object_representations (U) == true, "");
static_assert (__has_unique_object_representations (V) == false, "");
static_assert (__has_unique_object_representations (W) == false, "");
static_assert (sizeof (X) != sizeof (int) || __has_unique_object_representations (X) == true, "");
static_assert (__has_unique_object_representations (float) == false, "");
static_assert (__has_unique_object_representations (double) == false, "");
static_assert (__has_unique_object_representations (long double) == false, "");
static_assert (__has_unique_object_representations (void) == false, "");
static_assert (__has_unique_object_representations (_Complex int) == true, "");
static_assert (__has_unique_object_representations (_Complex float) == false, "");
static_assert (__has_unique_object_representations (_Complex double) == false, "");
static_assert (__has_unique_object_representations (_Complex long double) == false, "");
static_assert (__has_unique_object_representations (int __attribute__((vector_size (16)))) == true, "");
static_assert (__has_unique_object_representations (float __attribute__((vector_size (16)))) == false, "");
static_assert (__has_unique_object_representations (int X::*) == true, "");
static_assert (__has_unique_object_representations (void (X::*) ()) == true, "");
static_assert (__has_unique_object_representations (int *) == true, "");
static_assert (__has_unique_object_representations (int (*) ()) == true, "");
static_assert (__has_unique_object_representations (decltype (nullptr)) == false, "");
static_assert (__has_unique_object_representations (Y) == (sizeof (Y) == 3 * sizeof (char)), "");
static_assert (__has_unique_object_representations (Z) == false, "");
static_assert (__has_unique_object_representations (A) == false, "");
static_assert (sizeof (B) != sizeof (long) || __has_unique_object_representations (B) == true, "");
static_assert (__has_unique_object_representations (C) == false, "");
static_assert (__has_unique_object_representations (D) == false, "");
