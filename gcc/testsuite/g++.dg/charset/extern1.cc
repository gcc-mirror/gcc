/* Test extern statments not being translated. */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" }
*/

extern  "C" {


int testbug (void) {

  return 0;

}

} //extern block
