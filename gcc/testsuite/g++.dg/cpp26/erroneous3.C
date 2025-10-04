// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++11 } }
// { dg-options "-Wimplicit-fallthrough -Wswitch-unreachable" }
// Make sure -Wimplicit-fallthrough and -Wswitch-unreachable
// are consistent between -std=c++23 and -std=c++26 even when
// the latter instruments jumps across vacuous initializations.

int i;

void
foo (int x)
{
  switch (x)
    {
    case 1:
      int j;
      ++i;			// { dg-warning "this statement may fall through" }
    case 2:			// { dg-message "here" }
      int k;
      ++i;
      // FALLTHRU
    case 3:
      int l;
      ++i;
      [[fallthrough]];
    default:
      int m;
      ++i;
      j = 42;
      k = 42;
      l = 42;
      m = 42;
      i += (j - k) + (l - m);
      break;
    }
}

void
bar (int x)
{
  if (x == 6)
    goto l1;
  if (x == 7)
    goto l2;
  if (x == 8)
    goto l3;
  if (x == 9)
    goto l4;
  if (x == 10)
    goto l5;
  if (x == 11)
    goto l6;
  int j;
  j = 5;
  i += j;
  switch (x)
    {
    case 1:
    l1:
      ++i;			// { dg-warning "this statement may fall through" }
    case 2:			// { dg-message "here" }
    l2:
      ++i;
      // FALLTHRU
    case 3:
    l3:
      ++i;
      [[fallthrough]];
    default:
    l4:
      ++i;
      break;
    case 4:
      ++i;			// { dg-warning "this statement may fall through" }
    case 5:			// { dg-message "here" }
    l5:;
      ++i;			// { dg-warning "this statement may fall through" }
    case 6:			// { dg-message "here" }
      ++i;
    case 7:
    l6:;
    }
}

void
baz (int x)
{
  switch (x)
    {
    case 1:
      int j [[indeterminate]];
      ++i;			// { dg-warning "this statement may fall through" }
    case 2:			// { dg-message "here" }
      int k [[indeterminate]];
      ++i;
      // FALLTHRU
    case 3:
      int l [[indeterminate]];
      ++i;
      [[fallthrough]];
    default:
      int m [[indeterminate]];
      ++i;
      j = 42;
      k = 42;
      l = 42;
      m = 42;
      i += (j - k) + (l - m);
      break;
    }
}

void
qux (int x)
{
  if (x == 6)
    goto l1;
  if (x == 7)
    goto l2;
  if (x == 8)
    goto l3;
  if (x == 9)
    goto l4;
  if (x == 10)
    goto l5;
  if (x == 11)
    goto l6;
  int j [[indeterminate]];
  j = 5;
  i += j;
  switch (x)
    {
    case 1:
    l1:
      ++i;			// { dg-warning "this statement may fall through" }
    case 2:			// { dg-message "here" }
    l2:
      ++i;
      // FALLTHRU
    case 3:
    l3:
      ++i;
      [[fallthrough]];
    default:
    l4:
      ++i;
      break;
    case 4:
      ++i;			// { dg-warning "this statement may fall through" }
    case 5:			// { dg-message "here" }
    l5:;
      ++i;			// { dg-warning "this statement may fall through" }
    case 6:			// { dg-message "here" }
      ++i;
    case 7:
    l6:;
    }
}
