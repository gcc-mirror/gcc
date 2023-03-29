/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-evrp" } */


void kill();

static long a;
static unsigned b;
int test1 () {
    long c, e;
    c = b = a;
    e = c ? 2 / (c + 1) : 0;
    if (e && !b)
        kill ();
    a = 0;
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */

