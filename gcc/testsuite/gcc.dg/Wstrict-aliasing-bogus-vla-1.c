/* PR 41673: bogus -Wstrict-aliasing warning from VLA dereference.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2 -Wall" } */
/* { dg-require-effective-target alloca } */

int main(int argc, char *argv[])
{
    float x[argc];
    float y[argc];
    return 0 == __builtin_memcpy(y, x, argc * sizeof(*x));
}
