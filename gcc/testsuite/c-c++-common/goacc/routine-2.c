/* Test invalid use of the OpenACC 'routine' directive.  */

#pragma acc routine gang worker /* { dg-error "conflicting level" } */
void gang (void)
{
}

#pragma acc routine worker vector /* { dg-error "conflicting level" } */
void worker (void)
{
}

#pragma acc routine vector seq /* { dg-error "conflicting level" } */
void vector (void)
{
}

#pragma acc routine seq gang /* { dg-error "conflicting level" } */
void seq (void)
{
}

#pragma acc routine (nothing) gang /* { dg-error "not been declared" } */
