// PR c++/77803
// { dg-do compile { target c++11 } }
// { dg-options "-Wimplicit-fallthrough" }

struct A {};
int a;

void
fn1 ()
{
  switch (0) {
  case 0:
  {
    A b;
    [[fallthrough]];
  }
  default:
    a = 0;
  }
}

void
fn2 ()
{
  switch (0) {
  case 0:
  {
    A b;
  } // { dg-warning "statement may fall through" }
  default:
    a = 0;
  }
}
