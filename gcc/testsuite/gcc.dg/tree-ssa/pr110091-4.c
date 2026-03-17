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
    struct tEntry entry = { 0 };

    if (otherfunc(&entry) != 0)
      return;
    if (out)
     *out = entry.value; /* { dg-bogus "dangling pointer to" } */
    anotherfunc(5);
}

void foo()
{
    bar();
}

/* There should be 2 CLOBBERs, 1 for entry as they have been "sinked" and one for the inlined version.  */
/* { dg-final { scan-tree-dump-times "CLOBBER\\\(eos\\\)" 2 "optimized" } } */
/* { dg-final { scan-tree-dump "sinking common stores with same value to entry" "sink1" } } */

