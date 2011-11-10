/* { dg-do compile } */

void foo (void);
void bar (void *);
extern int t;

static void kmalloc_large (int size, int flags)
{
  (void) size;
  (void) flags;
  foo ();
  bar (({__here:&&__here;}));
}

static void kmalloc (int size, int flags)
{
  if (size)
    {
      if ((unsigned long) size > 0x1000)
	kmalloc_large (size, flags);

      if (flags)
	bar (({__here:&&__here;}));
    }
}

void compress_file_range (int i, int j, int k)
{
  int nr_pages = ({j < k;});

  if (i || t)
    kmalloc (0x1000UL * nr_pages, 0x40UL);
}
