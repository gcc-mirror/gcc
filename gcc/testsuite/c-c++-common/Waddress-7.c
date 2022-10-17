/* PR c/106947 */
/* { dg-do compile } */
/* { dg-options "-Waddress" } */

#ifndef __cplusplus
# define bool _Bool
#endif

#pragma GCC diagnostic ignored "-Waddress"
int s; /* { dg-bogus "declared" } */
bool e = &s;
int
main ()
{
  int error = 0;
  {
    bool e1 = &s;
    if (!e1)
      error = 1;
  }
  return error;
}
