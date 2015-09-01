// { dg-do compile }

struct S
{
  S f; // { dg-error "incomplete type" }
};

void
fn1 (S p1)
{
}
