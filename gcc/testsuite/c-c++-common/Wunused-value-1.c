/* PR c/97748 */
/* { dg-do compile } */
/* { dg-options "-Wunused-value" } */

double _Complex f ();
double _Complex *p;

double _Complex
foo (double _Complex x)
{
  ++x;			/* { dg-bogus "value computed is not used" } */
  --x;			/* { dg-bogus "value computed is not used" } */
  x += 1;		/* { dg-bogus "value computed is not used" } */
  x += 1.0iF;		/* { dg-bogus "value computed is not used" } */
  x++;			/* { dg-bogus "value computed is not used" } */
  x--;			/* { dg-bogus "value computed is not used" } */
  x + 1;		/* { dg-warning "value computed is not used" } */
  (void) (x + 1);	/* { dg-bogus "value computed is not used" } */
  1 + f (); 		/* { dg-warning "value computed is not used" } */
  f () + f (); 		/* { dg-warning "value computed is not used" } */
  f () + f (), f (); 	/* { dg-warning "value computed is not used" } */
  f ();
  (void) f ();
  *p++;			/* { dg-warning "value computed is not used" } */
  ++*p;			/* { dg-bogus "value computed is not used" } */
  (*p ? f () : 0);
  ({ f (); });
  ({ f () + 1; });
  ({ f (); 0; });
  ({ f () + 1; 0; });	/* { dg-warning "value computed is not used" } */
  1 + ({ f (); });	/* { dg-warning "value computed is not used" } */
  return x;
}
