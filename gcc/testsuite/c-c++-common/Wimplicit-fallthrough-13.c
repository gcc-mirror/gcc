/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* As per <http://security.coverity.com/blog/2013/Sep/gimme-a-break.html>, don't
   warn for terminated branches (fall through to break / end of the switch).  */

extern void bar (int);

void
f (int i)
{
  switch (i)
    {
    case 1:
      bar (1);
    default:
      return;
    }

  switch (i)
    {
    case 1:
      bar (1);
    default:
      goto X;
    }
X:

  switch (i)
    {
    case 1:
      bar (1);
    default:
      break;
    }

  switch (i)
    {
    case 1:
      bar (1);
    case 2:
    case 3:
    default:
      break;
    }

  switch (i)
    {
    case 1:
      bar (1);
    default:;
    }

  switch (i)
    {
    case 1:
      bar (1);
    case 2:
    case 3:
    default:;
    }
}
