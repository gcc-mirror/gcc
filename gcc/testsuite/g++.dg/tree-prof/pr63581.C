// { dg-require-effective-target freorder }
/* { dg-options "-O2 -g -fno-peel-loops" } */

struct page {
  int i;
} global;

__attribute__((noinline)) static struct page* find_page1 (int i)
{
  if ( i< 150)
      return 0;
  global.i = i;
  return &global;
}

__attribute__((noinline)) static struct page* find_page2 (int i)
{
  global.i = i;
  return &global;
}

volatile int ii;
__attribute__((noinline)) static int zero (void)
{
  return ii;
}

static inline int uptodate (struct page* p)
{
  return (p->i < 709);
}

static struct page* bar(int i)
{
  struct page *page;

repeat:
  page = find_page1 (i);
  if (!page) {
    page = find_page2 (i);
    if (!page)
      return 0;
    if (zero () ) {
      zero ();
      goto repeat;
    }
  }
  return page;
}

__attribute__((noinline)) int foo (int n)
{
  struct page *page;

retry:
  page = bar (n);
  if (page == 0)
    return 0;
  if (uptodate (page))
    goto out;

   zero ();
   if (page->i < 0) {
     zero ();
     goto retry;
   }
out:
   return 1;
}

__attribute__((noinline)) int hot (void)
{
  int i;
  int sum = 0;

  for (i = 0; i < 433038; i++)
    sum+=i;

  return sum;
}

int main(void)
{
  int i;

  global.i = hot ();
  for (i = 0; i < 858; i++)
    foo (i);

  return 0;
}
