typedef struct { int s, t; } C;
C x;
int foo (void);
void bar (int);

int baz (void)
{
  int a = 0, c, d = 0;
  C *b = &x;

  while ((c = foo ()))
    switch(c)
      {
      case 23:
	bar (1);
	break;
      default:
	break;
      }

  if (a == 0 || (a & 1))
    {
      if (b->s)
	{
	  if (a)
	    bar (1);
	  else
	    a = 16;
	}
      else if (b->t)
	{
	  if (a)
	    bar (1);
	  else
	    a = 32;
	}
    }

  if (d && (a & ~127))
    bar (2);
  return 0;
}
