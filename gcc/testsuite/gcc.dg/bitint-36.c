/* PR c/102989 */
/* { dg-do compile { target { bitint } } } */
/* { dg-options "-std=c23 -fpermissive -Wint-in-bool-context -Waddress -Wpointer-to-int-cast -Wint-to-pointer-cast -Wint-conversion -Wshift-negative-value -Wshift-count-overflow -Wdiv-by-zero" } */

extern char *ax[];

void bar (int);

void
foo (_BitInt(61) x, long long *y)
{
  if (x << 15)						/* { dg-warning "'<<' in boolean context, did you mean '<'" } */
    ;
  bar ((_BitInt(sizeof (void *) * __CHAR_BIT__)) (ax + 0) == 0);
  bar ((unsigned _BitInt(sizeof (void *) * __CHAR_BIT__)) (ax + 1) == 0);
  char *z = __builtin_assume_aligned (y, 32wb, 8wb);
  *z = 42;
  _BitInt(sizeof (void *) * __CHAR_BIT__ + 1) a = (_BitInt(sizeof (void *) * __CHAR_BIT__ + 1)) y;			/* { dg-warning "cast from pointer to integer of different size" } */
  unsigned _BitInt(sizeof (void *) * __CHAR_BIT__ + 1) b = (unsigned _BitInt(sizeof (void *) * __CHAR_BIT__ + 1)) y;	/* { dg-warning "cast from pointer to integer of different size" } */
  _BitInt(sizeof (void *) * __CHAR_BIT__ - 1) c = (_BitInt(sizeof (void *) * __CHAR_BIT__ - 1)) y;			/* { dg-warning "cast from pointer to integer of different size" } */
  unsigned _BitInt(sizeof (void *) * __CHAR_BIT__ - 1) d = (unsigned _BitInt(sizeof (void *) * __CHAR_BIT__ - 1)) y;	/* { dg-warning "cast from pointer to integer of different size" } */
  void *e = (void *) a;					/* { dg-warning "cast to pointer from integer of different size" } */
  void *f = (void *) b;					/* { dg-warning "cast to pointer from integer of different size" } */
  void *g = (void *) c;					/* { dg-warning "cast to pointer from integer of different size" } */
  void *h = (void *) d;					/* { dg-warning "cast to pointer from integer of different size" } */
  a = y;						/* { dg-warning "assignment to '_BitInt\\\(\[0-9]+\\\)' from 'long long int \\\*' makes integer from pointer without a cast" } */
  b = y;						/* { dg-warning "assignment to 'unsigned _BitInt\\\(\[0-9]+\\\)' from 'long long int \\\*' makes integer from pointer without a cast" } */
  c = y;						/* { dg-warning "assignment to '_BitInt\\\(\[0-9]+\\\)' from 'long long int \\\*' makes integer from pointer without a cast" } */
  d = y;						/* { dg-warning "assignment to 'unsigned _BitInt\\\(\[0-9]+\\\)' from 'long long int \\\*' makes integer from pointer without a cast" } */
  e = a;						/* { dg-warning "assignment to 'void \\\*' from '_BitInt\\\(\[0-9]+\\\)' makes pointer from integer without a cast" } */
  f = b;						/* { dg-warning "assignment to 'void \\\*' from 'unsigned _BitInt\\\(\[0-9]+\\\)' makes pointer from integer without a cast" } */
  g = c;						/* { dg-warning "assignment to 'void \\\*' from '_BitInt\\\(\[0-9]+\\\)' makes pointer from integer without a cast" } */
  h = d;						/* { dg-warning "assignment to 'void \\\*' from 'unsigned _BitInt\\\(\[0-9]+\\\)' makes pointer from integer without a cast" } */
  _BitInt(61) i = (1wb - 1152921504606846975wb) << 1;	/* { dg-warning "left shift of negative value" } */
							/* { dg-warning "result of '-1152921504606846974 << 1' requires 62 bits to represent, but '_BitInt\\\(61\\\)' only has 61 bits" "" { target *-*-* } .-1 } */
  _BitInt(61) j = i << (60 + 1);			/* { dg-warning "left shift count >= width of type" } */
  _BitInt(61) k = i >> (60 + 1);			/* { dg-warning "right shift count >= width of type" } */
  _BitInt(61) l = i / (51wb - 51wb);			/* { dg-warning "division by zero" } */
}
