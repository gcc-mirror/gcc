/* PR c/78775 - [7 Regression] ICE in maybe_warn_alloc_args_overflow
   { dg-do compile }
   { dg-options "-O2" } */

int a, b, *c;

int main (void)
{
  unsigned long d = 0;
  while (1)
    {
      switch (b)
      case 'S':
	d = a;
      c = __builtin_malloc (d);
    }

  return 0;
}
