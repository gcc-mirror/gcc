/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

extern void bar (int);
extern void die (void) __attribute__((noreturn));

/* Test may_fallthru-ness.  */

void
f (int i)
{
  switch (i)
    {
    case 1:
      bar (0);
      __attribute__((fallthrough));
    case 2:;
    }

  switch (i)
    {
    case 1:
      bar (0);
      return;
    case 2:;
    }

  switch (i)
    {
    case 1:
      bar (0);
      break;
    case 2:;
    }

  switch (i)
    {
    case 1:
      bar (0);
      goto L1;
L1:
    case 2:;
    }

  switch (i)
    {
    case 1:
      bar (0);
      die ();
    case 2:;
    }

  switch (i)
    {
    case 1:
      {
	int i, j, k;
	bar (0);
	__attribute__((fallthrough));
      }
    case 2:;
    }

  switch (i)
    {
    case 1:
      {
	int i, j, k;
        bar (0);
        return;
      }
    case 2:;
    }

  switch (i)
    {
    case 1:
      {
	int i, j, k;
        bar (0);
        break;
      }
    case 2:;
    }

  switch (i)
    {
    case 1:
      {
	int i, j, k;
        bar (0);
        goto L2;
      }
L2:
    case 2:;
    }

  switch (i)
    {
    case 1:
      {
	int i, j, k;
        bar (0);
        die ();
      }
    case 2:;
    }
}
