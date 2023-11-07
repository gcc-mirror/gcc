/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

void
foo (int x)
{
  _BitInt (1) a;		/* { dg-error "'signed _BitInt' argument must be at least 2" } */
  signed _BitInt (1) b;		/* { dg-error "'signed _BitInt' argument must be at least 2" } */
  _BitInt (0) c;		/* { dg-error "'_BitInt' argument '0' is not a positive integer constant expression" } */
  unsigned _BitInt (0) d;	/* { dg-error "'_BitInt' argument '0' is not a positive integer constant expression" } */
  _BitInt (-42) e;		/* { dg-error "'_BitInt' argument '-42' is not a positive integer constant expression" } */
  _BitInt (-5) unsigned f;	/* { dg-error "'_BitInt' argument '-5' is not a positive integer constant expression" } */
  _BitInt (4294967295ULL) g;	/* { dg-error "'_BitInt' argument '4294967295' is larger than 'BITINT_MAXWIDTH' '\[0-9]+'" } */
  long _BitInt (42) h;		/* { dg-error "both 'long' and '_BitInt' in declaration specifiers" } */
  long long _BitInt (42) i;	/* { dg-error "both 'long' and '_BitInt' in declaration specifiers" } */
  _BitInt (42) long j;		/* { dg-error "both 'long' and '_BitInt' in declaration specifiers" } */
  _BitInt (42) long long k;	/* { dg-error "both 'long' and '_BitInt' in declaration specifiers" } */
  short _BitInt (42) l;		/* { dg-error "both 'short' and '_BitInt' in declaration specifiers" } */
  _BitInt (42) short m;		/* { dg-error "both 'short' and '_BitInt' in declaration specifiers" } */
  _Complex _BitInt (42) n;	/* { dg-error "both 'complex' and '_BitInt' in declaration specifiers" } */
  _BitInt (42) _Complex o;	/* { dg-error "both 'complex' and '_BitInt' in declaration specifiers" } */
  int _BitInt (42) p;		/* { dg-error "two or more data types in declaration specifiers" } */
  _BitInt (42) int q;		/* { dg-error "two or more data types in declaration specifiers" } */
  _BitInt (x) r;		/* { dg-error "'_BitInt' argument is not an integer constant expression" } */
}
