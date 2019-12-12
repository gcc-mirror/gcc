// PR bootstrap/89560
// { dg-do compile }

#define TEN(x) x##0, x##1, x##2, x##3, x##4, x##5, x##6, x##7, x##8, x##9,
#define HUNDRED(x) TEN(x##0) TEN(x##1) TEN(x##2) TEN(x##3) TEN(x##4) \
		   TEN(x##5) TEN(x##6) TEN(x##7) TEN(x##8) TEN(x##9)
int foo (int, ...);

int
bar ()
{
  return (foo (HUNDRED (1) 0));
}
