/* { dg-do compile } */
/* { dg-options "-O2 -falign-functions=100000" } */
/* { dg-error "is not between 0 and 65536" "" { target *-*-* } 0 } */

void
test_func (void)
{
}
