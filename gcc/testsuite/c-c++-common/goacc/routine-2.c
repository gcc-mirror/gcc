#pragma acc routine gang worker /* { dg-error "multiple loop axes" } */
void gang (void)
{
}

#pragma acc routine worker vector /* { dg-error "multiple loop axes" } */
void worker (void)
{
}

#pragma acc routine vector seq /* { dg-error "multiple loop axes" } */
void vector (void)
{
}

#pragma acc routine seq gang /* { dg-error "multiple loop axes" } */
void seq (void)
{
}

#pragma acc routine (nothing) gang /* { dg-error "not been declared" } */
