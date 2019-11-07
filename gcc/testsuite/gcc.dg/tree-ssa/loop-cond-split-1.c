/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-lsplit-details" } */

extern const int step;

int ga, gb;

__attribute__((pure)) __attribute__((noinline)) int inc (int i)
{
  return i + step;
}

extern int do_something (void);

void test1 (int n)
{
  int i;

  for (i = 0; i < n; i = inc (i))
    {
      if (ga)
        ga = do_something ();
    }
}

void test2 (int n, int p)
{
  int i;
  int v;

  for (i = 0; i < n ; i = inc (i))
    {
      if (ga)
       {
         v = inc (2);
         gb += 1;
       }
      else
       {
         v = p * p;
         gb *= 3;
       }

      if (v < 10)
        ga = do_something ();
    }
}

void test3 (int n, int p)
{
  int i;
  int c = p + 1;
  int v;

  for (i = 0; i < n ; i = inc (i))
    {
      if (c)
       {
         v = inc (c);
         gb += 1;
       }
      else
       {
         v = p * p;
         gb *= 3;
       }

      if (v < 10)
        c = do_something ();
    }
}

void test4 (int n, int p)
{
  int i;
  int v;

  for (i = 0; i < n ; i = inc (i))
    {
      if (ga)
       {
         v = inc (2);
         if (gb > 16)
           v = inc (5);  
       }
      else
       {
         v = p * p;
         gb += 2;
       }

      if (v < 10)
        ga = do_something ();
    }
}

/* { dg-final { scan-tree-dump-times "loop split on semi-invariant condition at false branch" 3 "lsplit" } } */
