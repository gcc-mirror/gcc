// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR137: static_cast of cv void*

struct A;

const void* v;

void foo(void)
{
  (void)static_cast<const volatile A *>(v);
  (void)static_cast<A *>(v);  // { dg-error "" "static_cast cannot remove cv qualifiers" }
}
