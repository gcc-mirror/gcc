// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -g" }

void
foo ()
{
  constexpr auto r = ^^int;
}
