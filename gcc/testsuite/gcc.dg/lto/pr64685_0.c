/* { dg-lto-do run } */
/* { dg-lto-options { { -flto } } } */

extern int b;

void
fn1 (void)
{
    b = 0;
}
