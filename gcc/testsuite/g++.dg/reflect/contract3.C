// C++26 P3598R0 - CWG 3158 - const-ification of Splice Expressions
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

int v;

void
foo ()
  pre ((++[:object_of (^^v):], true))
{
}

void
bar ()
  pre ((++[:std::meta::reflect_object (v):], true))
{
}

template <bool B>
void
baz (int p)
  pre ((++[:object_of (B ? ^^v : ^^p):], true))
{
}

template <bool B>
void
qux (int p)
  pre ((++[:std::meta::reflect_object (B ? v : p):], true))
{
}

void
corge (int p)
{
  baz <true> (p);
  qux <true> (p);
}
