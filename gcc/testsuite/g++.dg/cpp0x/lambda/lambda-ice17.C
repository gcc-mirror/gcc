// PR c++/71570
// { dg-do compile { target c++11 } }

void foo (int);

void foo (void)
{
  [&foo] // { dg-error "cannot capture" }
  {
    foo (0);
  };
}
