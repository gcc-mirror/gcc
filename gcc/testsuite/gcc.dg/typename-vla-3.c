/* { dg-do run } 
 * { dg-options "-std=c99" }
 * */

static char tmp[2];

static int f(int n, char (*x)[sizeof *(++n, (struct { char (*x)[n]; }){ &tmp }).x]) /* { dg-warning "anonymous struct" } */
{
  return sizeof *x;
}

int main (void)
{
  if (2 != f(1, &tmp))
    __builtin_abort ();
  return 0;
}

