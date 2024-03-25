/* Tests the warnings for insufficient allocation size.
   { dg-do compile }
   { dg-options "-Walloc-size" }
 * */

struct b { int x[10]; };

void fo0(void)
{
        struct b *p = __builtin_malloc(sizeof *p);
}

void fo1(void)
{
        struct b *p = __builtin_malloc(sizeof p);	/* { dg-warning "allocation of insufficient size" } */
}

void fo2(void)
{
        struct b *p = __builtin_alloca(sizeof p);	/* { dg-warning "allocation of insufficient size" } */
}

void fo3(void)
{
        struct b *p = __builtin_calloc(1, sizeof p);	/* { dg-warning "allocation of insufficient size" } */
}

void g(struct b* p);

void fo4(void)
{
        g(__builtin_malloc(4));				/* { dg-warning "allocation of insufficient size" } */
}

