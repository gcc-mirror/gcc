/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-fPIC" } */
char *ptr = 0;
char array[100];
void
f()
{
  ptr = &array[0x100000000ULL];	/* A 33-bit constant.  */
}
