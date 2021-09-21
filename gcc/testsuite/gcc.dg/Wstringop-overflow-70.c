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
    /* The size of the write below depends on the target.  When vectorized
       the vector size may be 4, 8 or 16, otherwise it may be a series of byte
       assignments.  */
    p[i] = i;       // { dg-warning "writing (1|2|4|8|16) bytes? into a region of size (0|1|3|7|15)" }
  return p;
}
