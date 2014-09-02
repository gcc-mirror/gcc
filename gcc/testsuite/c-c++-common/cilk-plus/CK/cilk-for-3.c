/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

void bar (int *, int *, int *, int *);
void baz (char **, char **, char **, int *);

void
foo ()
{
  int a, b, c;
  char *d, *e;
  bar (0, &a, &b, &c);
  _Cilk_for (int i = a; i < b; i += c)
    bar (&i, &a, &b, &c);
  baz (0, &d, &e, &c);
  _Cilk_for (char *p = d; p != e; p += c)
    baz (&p, &d, &e, &c);
}
