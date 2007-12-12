/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple" } */

struct foo {
        unsigned a1: 1;
        unsigned a2: 3;
        unsigned : 4;
};

extern struct foo thefoo;

void setup_foo(void)
{
        const struct foo init = {
                .a1 = 1,
                .a2 = 5,
        };
        thefoo = init;
}

/* { dg-final { scan-tree-dump-times "thefoo.0 = \{\}" 1 "gimple"} } */
/* { dg-final { scan-tree-dump-times "thefoo.0.a1 = 1" 1 "gimple"} } */
/* { dg-final { scan-tree-dump-times "thefoo.0.a2 = 5" 1 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
