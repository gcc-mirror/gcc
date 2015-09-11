/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct abc {
    void (*abc_call)(void);
};

/*
 * Use only any one of the three definitions below at a time:
 *
 * 1. nothing optimized away. Good.
 * 2. call_func() _not_ optimized away, but struct xyz is. gcc disappoints.
 * 3. both call_func() and struct xyz optimized away. Nice.
 */

/* 1 */
/*extern int do_register(struct abc *xyz);*/

/* 2 */
static inline int do_register(struct abc *xyz)
{
    return 0;
}

/* 3 */
/*#define do_register(xyz)    do { (void)(xyz); } while (0)*/

static void call_func(void)
{
}

static struct abc xyz = {
    .abc_call = call_func,
};

void func(void)
{
    do_register(&xyz);
}

/* { dg-final { scan-tree-dump-not "call_func" "optimized"} } */
