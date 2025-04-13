/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef void (*fn_t) (void *) __attribute__ ((preserve_none));

__attribute__ ((preserve_none))
void
foo (void *frame)
{
}

fn_t func = foo;
