/* PR tree-optimization/79472 */
/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do compile { target nonpic } } */

void
foo (unsigned int v)
{
  const char *s;
  
  switch (v)
    {
    case 0:
      s = "foo";
      break;
    case 1:
      s = "bar";
      break;
    case 2:
      s = "spam";
      break;
    default:
      for (int i = 0; i < v; i++)
        __builtin_printf ("baz\n");
      return;
    }

  __builtin_printf ("%s\n", s);
}

int
bar (unsigned int v, int w)
{
  const char *s;
  
  switch (v)
    {
    case 0:
      s = "foo";
      break;
    case 1:
      s = "bar";
      break;
    case 2:
      s = "spam";
      break;
    default:
      __builtin_printf ("baz\n");
      if (v > 25)
	__builtin_printf ("bl1\n");
      else
	__builtin_printf ("bl2\n");
      goto lab;
    }

  __builtin_printf ("%s\n", s);
  if (w > 25)
    __builtin_printf ("cl1\n");
  else
    __builtin_printf ("cl2\n");
 lab:
  __builtin_printf ("dl\n");
  return v + w;
}

/* { dg-final { scan-tree-dump-times "Switch converted" 2 "switchconv" } } */
/* { dg-final { scan-tree-dump-times "= CSWTCH" 2 "switchconv" } } */
