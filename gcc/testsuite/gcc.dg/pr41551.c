/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Make sure we do not ICE.  */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

int main(void)
{
 int var, *p = &var;
 return (double)(uintptr_t)(p);
}

/* { dg-prune-output "-Wreturn-local-addr" } */
