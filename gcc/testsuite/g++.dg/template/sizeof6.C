// { dg-do compile }
// Contributed by Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// PR c++/13683: bogus warning about passing non-PODs through ellipsis

struct B {};
struct NonPOD : B {};

struct A
{
  static int check(...);
  static NonPOD GetNonPOD(void);
  enum { value = sizeof(A::check(A::GetNonPOD())) };
};
