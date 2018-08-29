/* { dg-options "-fdiagnostics-show-caret" } */

void bar(void);
void foo(void)
{
  bar() /* { dg-error "expected ';' before '.' token" } */
}
/* { dg-begin-multiline-output "" }
   bar()
        ^
        ;
 }
 ~       
   { dg-end-multiline-output "" } */
