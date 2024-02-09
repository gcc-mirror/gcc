/* Test minrt checks */

/* { dg-options "-O1 -minrt" } */

int main(int argc, char *argv[]) 
/* { dg-error "function 'main' must have no arguments when using the '-minrt' option" "" { target pru-*-* } .-1 } */
{
  for (;;)
    ;
}
