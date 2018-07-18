// PR c++/77886
// { dg-do compile }
// { dg-options "-Wimplicit-fallthrough" }

template <int N>
int
foo (int x, int y)
{
  switch (x)
    {
    case 1:
      x++;		// { dg-bogus "this statement may f\[ahlotu\]*gh" }
      // FALLTHROUGH
    case 2:
      x++;
      break;
    case 3:
      x++;		// { dg-bogus "this statement may f\[ahlotu\]*gh" }
      // FALLTHROUGH
    lab:
    case 4:
      x++;
      break;
    case 5:
      x++;		// { dg-bogus "this statement may f\[ahlotu\]*gh" }
      // FALLTHROUGH
    default:
      x++;
      break;
    case 26:
      goto lab;
    }
#if __cplusplus >= 201103L
  switch (y)
    {
    case 1:
      y++;		// { dg-bogus "this statement may f\[ahlotu\]*gh" }
      [[fallthrough]];
    case 2:
      y++;
      break;
    case 3:
      y++;		// { dg-bogus "this statement may f\[ahlotu\]*gh" }
      [[fallthrough]];
    lab2:
    case 4:
      y++;
      break;
    case 5:
      y++;		// { dg-bogus "this statement may f\[ahlotu\]*gh" }
      [[fallthrough]];
    default:
      y++;
      break;
    case 26:
      goto lab2;
    }
#endif
  return x + y;
}

int
bar (int x, int y)
{
  return foo<0> (x, y);
}
