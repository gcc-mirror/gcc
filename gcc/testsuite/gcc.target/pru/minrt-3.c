/* Test minrt checks */

/* { dg-options "-O1 -minrt" } */

int main(void)
/* { dg-error "function 'main' must never return when using the '-minrt' option" "" { target pru-*-* } .-1 } */
{
    return 0;
}
