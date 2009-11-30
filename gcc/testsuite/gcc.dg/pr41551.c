/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Make sure we do not ICE.  */

int main(void)
{
 int var, *p = &var;
 return (double)(unsigned long)(p);
}
