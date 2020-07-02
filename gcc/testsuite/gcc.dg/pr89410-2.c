/* { dg-options "-fdiagnostics-show-caret" } */

int main(void)
{
  /* This is 0x7fffffffffffffff, which truncates to 0xffffffff.  */
#line 9223372036854775807 /* { dg-warning "line number out of range" } */
#warning msg
  /* { dg-begin-multiline-output "" }
 #line 9223372036854775807
       ^~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-warning "msg" "" { target *-*-* } { -1 } } */
}
