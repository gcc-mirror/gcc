/* This was failing on Alpha because the comparison (p != -1) was rewritten
   as (p+1 != 0) and p+1 isn't allowed to wrap for pointers.  */

extern void abort(void);

typedef __SIZE_TYPE__ size_t;

int global;

static void *foo(int p)
{
  if (p == 0)
   {
      global++;
      return &global;
   }

  return (void *)(size_t)-1;
}

int bar(void)
{
  void *p;

  p = foo(global);
  if (p != (void *)(size_t)-1)
    return 1;

  global++;
  return 0;
}

int main(void)
{
  global = 1;
  if (bar () != 0)
    abort();
  
  return 0;
}
