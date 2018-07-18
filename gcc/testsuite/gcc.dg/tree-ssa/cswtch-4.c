/* PR tree-optimization/79472 */
/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do compile { target nonpic } } */

void
frobulate (unsigned int v)
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
      __builtin_abort ();
      break;
    }

  __builtin_printf ("%s\n", s);
}

void
frobulate_for_gcc (unsigned int v)
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
      s = (const char *) 0;
      break;
    }
  
  if (!s)
    __builtin_abort ();
  
  __builtin_printf ("%s\n", s);
}

/* { dg-final { scan-tree-dump-times "Switch converted" 2 "switchconv" } } */
/* { dg-final { scan-tree-dump-times "= CSWTCH" 2 "switchconv" } } */
