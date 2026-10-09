// PR c++/127780
// { dg-do compile { target c++20 } }

void
g ()
{
  if (using T = int; T t = 1)  // { dg-error "7:only available with" "" { target c++20_only } }
    ;
}
