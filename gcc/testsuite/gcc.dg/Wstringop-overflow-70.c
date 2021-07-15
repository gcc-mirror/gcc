/* PR tree-optimization/97027 - missing warning on buffer overflow storing
   a larger scalar into a smaller array
   Verify overflow by vector stores.
   { dg-do compile }
   { dg-options "-O3" } */

void* nowarn_loop (void)
{
  char *p = __builtin_malloc (16);
  for (int i = 0; i != 16; ++i)
    p[i] = i;
  return p;
}

void* warn_loop (void)
{
  char *p = __builtin_malloc (15);
  for (int i = 0; i != 16; ++i)
    p[i] = i;       // { dg-warning "writing 16 bytes into a region of size 15" }
  return p;
}
