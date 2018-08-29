/* PR c/83989 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wrestrict" } */

__attribute__((__malloc__)) extern void *my_malloc (__SIZE_TYPE__);
void baz (void *);

#define SIZE 32

void
foo (void)
{
  void *recmem = __builtin_malloc (SIZE);
  baz (recmem);
  while (1)
    {
      void *oldrecmem = recmem;
      recmem = __builtin_malloc (SIZE);
      if (!recmem)
	{
	  __builtin_free (oldrecmem);
	  return;
	}
      __builtin_memcpy (recmem, oldrecmem, SIZE);	/* { dg-bogus "accessing" } */
      baz (recmem);
      __builtin_free (oldrecmem);
    }
}

void
bar (void)
{
  void *recmem = my_malloc (SIZE);
  baz (recmem);
  while (1)
    {
      void *oldrecmem = recmem;
      recmem = my_malloc (SIZE);
      if (!recmem)
	{
	  __builtin_free (oldrecmem);
	  return;
	}
      __builtin_memcpy (recmem, oldrecmem, SIZE);	/* { dg-bogus "accessing" } */
      baz (recmem);
      __builtin_free (oldrecmem);
    }
}
