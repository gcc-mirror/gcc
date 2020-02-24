// PR c++/93712 - ICE with ill-formed array list-initialization.
// { dg-do compile { target c++11 } }

int f (const int (&)[2]);

int g ()
{
  const int (&r)[2] = {1, "foo"}; // { dg-error "conversion" }
  return f({1, "foo"}); // { dg-error "conversion" }
}
