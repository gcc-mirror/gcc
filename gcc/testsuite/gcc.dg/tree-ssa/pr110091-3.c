/* { dg-do compile } */
/* { dg-options "-Wall -O2 -fdump-tree-optimized -fdump-tree-sink1-details" } */
/* PR tree-optimization/110091 */
/* The clobbers are after the outlined code */

struct tEntry
{
    int value;
};
int *out;

extern int otherfunc(struct tEntry *);
extern void anotherfunc(int val);

void bar()
{
    struct tEntry entry1 = { 0 };
    struct tEntry entry2 = { 0 };

    if (otherfunc(&entry2) != 0)
      return;
    if (otherfunc(&entry1) != 0)
      return;
    if (out)
     *out = entry2.value; /* { dg-bogus "dangling pointer to" } */
    anotherfunc(5);
}

void foo()
{
    bar();
}

/* There should be 2 CLOBBERs, 1 for each: entry1 and entry2 as they have been "sinked".  */
/* { dg-final { scan-tree-dump-times "CLOBBER\\\(eos\\\)" 2 "optimized" } } */
/* { dg-final { scan-tree-dump "sinking common stores with same value to entry2" "sink1" } } */
/* { dg-final { scan-tree-dump "sinking common stores with same value to entry1" "sink1" } } */

