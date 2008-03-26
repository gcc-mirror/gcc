/* { dg-do link } */
/* { dg-options "-O2" } */

/* Test to check whether global variables are being
   constant propagated. */

extern void link_error (void);

int G;

void
foo (int i)
{
   if (i > 0)
     G = 3;
   else
     G = 3;

   if (G != 3)
     link_error ();
}

int
main ()
{
   foo (0);
   return 0;
}

