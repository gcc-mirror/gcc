/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */

void *
test (unsigned long long x, unsigned long long y)
{
    return (void *) (unsigned int) (x / y);
}
