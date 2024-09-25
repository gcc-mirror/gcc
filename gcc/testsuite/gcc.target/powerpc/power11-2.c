/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check if we can set the power11 target via a target attribute.  */

__attribute__((__target__("cpu=power9")))
void foo_p9 (void)
{
}

__attribute__((__target__("cpu=power10")))
void foo_p10 (void)
{
}

__attribute__((__target__("cpu=power11")))
void foo_p11 (void)
{
}
