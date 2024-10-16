/* { dg-options "-Wno-old-style-definition -fdiagnostics-show-caret" } */

int i /* { dg-error "6: expected ';' before 'int'" } */
int j;
/* { dg-begin-multiline-output "" }
 int i 
      ^
      ;
 int j;
 ~~~   
   { dg-end-multiline-output "" } */


void test (void)
{
  int i /* { dg-error "8: expected ';' before 'int'" } */
  int j;

  /* { dg-begin-multiline-output "" }
   int i 
        ^
        ;
   int j;
   ~~~   
     { dg-end-multiline-output "" } */
}

int old_style_params (first, second)
     int first;
     int second;
{
  return first + second;
}
