/* PR middle-end/89230 - Bogus uninited usage warning with printf
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct S { int i, j; };

/* attribute__ ((malloc)) */ struct S* f (void);

int g (void)
{
  struct S *p = f ();
  struct S *q; // { dg-bogus "" "uninitialized" { xfail *-*-* } }

  if (p->i || !(q = f ()) || p->j != q->i)
   {
     __builtin_printf ("%i", p->i);

     if (p->i)
       return 1;

     if (!q)        // { dg-bogus "\\\[-Wmaybe-uninitialized" "" { xfail *-*-* } }
       return 2;
   }

  return 0;
}
