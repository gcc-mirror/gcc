// { dg-do compile }
// { dg-options -Wparentheses }

// C++ version of gcc.dg/Wparentheses-1.c.

int foo (int a, int b)
{
  int c = (a && b) || 0;	// { dg-bogus "suggest parentheses" }
  c = a && b || 0;		// { dg-warning "suggest parentheses" }

  return (a && b && 1) || 0;	// { dg-bogus "suggest parentheses" }
}
