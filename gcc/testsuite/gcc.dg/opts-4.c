/* --dump= should translate to -d with joined operand.  */
/* { dg-do compile } */
/* { dg-options "--dump=a" } */

void f (void)
{
}

/* { dg-final { cleanup-rtl-dump "*" } } */
