/* Test C23 constexpr.  Valid code using GNU extensions, compilation tests.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

struct s { struct { int x, y; } x; };
constexpr struct s v = { { 123, 150 } };
int k;
constexpr int a[200] = { [v.x.x ... v.x.y] = 7 };

void
f ()
{
  switch (k)
    {
    case v.x.x ... v.x.y: ;
    }
}
