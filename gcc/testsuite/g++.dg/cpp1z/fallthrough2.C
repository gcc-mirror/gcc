// { dg-do compile { target c++17 } }
// { dg-options "-Wextra -Wall -Wpedantic" }

int
foo (int i)
{
  switch (i)
    {
    case 2:
      ++i;
      [[fallthrough, whatever::fallthrough]];		// { dg-bogus "attribute 'fallthrough' specified multiple times" }
    case 3:						// { dg-warning "'fallthrough' attribute ignored" "" { target *-*-* } .-1 }
      ++i;
      [[fallthrough, whatever2::fallthrough(1, 2, 3)]];	// { dg-bogus "attribute 'fallthrough' specified multiple times" }
    case 4:						// { dg-warning "'fallthrough' attribute ignored" "" { target *-*-* } .-1 }
      [[whatever3::fallthrough("abcd")]];		// { dg-warning "attributes at the beginning of statement are ignored" }
    case 5:
      [[whatever4::fallthrough]];			// { dg-bogus "attribute 'fallthrough' not preceding a case label or default label" }
      ++i;						// { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
    default:
      break;
    }
  return i;
}
