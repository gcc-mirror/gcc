int *
x(void)
{
 register int *a asm("unknown_register");  /* { dg-error "invalid register" } */
 int *v[1] = {a};
 return v[1];
}
