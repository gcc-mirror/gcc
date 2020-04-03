/* { dg-do compile } */

typedef volatile struct {
    int a;
    char *b;
} __attribute__((uncached)) my_type_t;

my_type_t x;

void foo (my_type_t *p)
{
    p->a = 10;
    p->b = 0;
}

void bar (void)
{
    x.a = 10;
    x.b = 0;
}

/* { dg-final { scan-assembler-times "st\.di" 4 } } */
