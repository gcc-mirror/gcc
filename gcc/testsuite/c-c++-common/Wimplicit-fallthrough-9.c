/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Test we don't remove FALLTHROUGH () too early.  */

extern void h (int);

void
g (int i)
{
  switch (i)
    {
    case 1:
      {
	switch (i)
	  {
	    case 3:
	      h (7);
	      __attribute__((fallthrough));
	    case 4:;
	  }
      }
    case 2:;
    }
}
