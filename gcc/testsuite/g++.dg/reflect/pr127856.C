// PR c++/127856
// { dg-do compile { target c++26 } }
// { dg-additional-options "-O2 -freflection" }

struct A { decltype (^^int) a; bool b = true; };

void
foo ()
{
  A a;
}
