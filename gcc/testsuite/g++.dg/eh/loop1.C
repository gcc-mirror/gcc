// Verify that loop optimization takes into account the exception edge
// and does not increment I before the call.
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort();
static void bar(char *);

static void foo(unsigned long element_count, char *ptr)
{
  unsigned long i;
  try {
    for (i = 0; i != element_count; i++, ptr += 8)
      bar (ptr);
  }
  catch (...) {
    if (i)
      abort ();
  }
}

static void bar(char *)
{
  throw 1;
}

int main()
{
  foo(2, 0);
}
