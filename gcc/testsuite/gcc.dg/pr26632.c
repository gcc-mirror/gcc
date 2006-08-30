/* PR middle-end/26632
   We used to issue a warning for an implicit cast whose result is not
   used.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

int g (void);
long h (void);

void
f (void)
{
  0 ? h () : g (); /* { dg-bogus "value computed is not used" } */
}
