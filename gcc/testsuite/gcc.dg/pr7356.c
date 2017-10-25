/* { dg-options "-fdiagnostics-show-caret" } */

a /* { dg-line stray_token } */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
int main(int argc, char** argv)
{
  return 0;
}

/* { dg-error "expected ';' before '.*'" "" { target *-*-* } stray_token } */
/* { dg-begin-multiline-output "" }
 a
  ^
  ;
   { dg-end-multiline-output "" } */
