/* { dg-do compile } */
/* { dg-options "-Wall -O2 -fdump-tree-optimized" } */
/* PR tree-optimization/110091 */
/* The clobbers are before the outlined code */

struct tEntry
{
    int value;
};
int *out;

extern int otherfunc(struct tEntry *);
extern void anotherfunc(int val);

void bar()
{
  {
    struct tEntry entry1 = { 0 };
    struct tEntry entry = { 0 };

    if (otherfunc(&entry) != 0)
      return;
    if (otherfunc(&entry1) != 0)
      return;
    if (out)
     *out = entry.value; /* { dg-bogus "dangling pointer to" } */
  }
  anotherfunc(5);
}

void foo()
{
    bar();
}

/* There should be 4 CLOBBERs, 2 for entry1 and 2 for entry.  */
/* { dg-final { scan-tree-dump-times "CLOBBER\\\(eos\\\)" 4 "optimized" } } */

