// { dg-do assemble  }
// g++ 1.36.1 bug 900211_04

// g++ fails to flag as errors attempts to compare pointer values against
// (nonzero) integer values;

// Since implicit conversions of pointer to integers (or vise versa) are
// illegal, these comparisons are also illegal.

// Cfront 2.0 passes this test.

// keywords: comparison operators, pointer types, integral types

int result;
int i;
char *p;

void function ()
{
  result = i == p;	/* { dg-error "" } caught by g++ */
  result = i != p;	/* { dg-error "" } caught by g++ */
  result = i >  p;	/* { dg-error "" } missed */
  result = i <  p;	/* { dg-error "" } missed */
  result = i >= p;	/* { dg-error "" } missed */
  result = i <= p;	/* { dg-error "" } missed */
}

int main () { return 0; }
