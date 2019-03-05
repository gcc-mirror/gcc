/* { dg-options "" } */

int main(void)
{
  /* This is 0xffffffff.  */
#line 4294967295
#warning msg
  /* { dg-warning "msg" "" { target *-*-* } -1 } */
}
