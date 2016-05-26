/* { dg-do compile } */
/* { dg-options "-g" } */

/* Make sure we didn't eliminate foo because we thought it was unused.  */

struct foo 
{
    int i;
};

int bar (void)
{
    return ((struct foo *)0x1234)->i;
}

/* { dg-final { scan-assembler "foo" { xfail nvptx-*-* } } } */
