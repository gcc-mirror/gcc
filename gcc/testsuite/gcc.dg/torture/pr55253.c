/* { dg-do run } */

struct
{
    int mallocFailed;
}
*a;

struct StrAccum
{
    int useMalloc;
}
b, c;

static void
fn1 (struct StrAccum *p1, int p2)
{
    if (p2 == 0)
        return;
    if (p1->useMalloc)
        a->mallocFailed = 0;
}

void
fn2 (struct StrAccum *p1)
{
    fn1 (p1, 1);
}

void
fn3 (struct StrAccum *p1)
{
    fn1 (p1, 1);
}

void
fn4 ()
{
    c.useMalloc = 1;
    fn1 (&c, 0);
}

int
main ()
{
    fn3 (&b);
    return 0;
}
