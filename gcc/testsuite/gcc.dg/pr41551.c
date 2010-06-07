/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Make sure we do not ICE.  */

__extension__ typedef __SIZE_TYPE__ size_t;

int main(void)
{
 int var, *p = &var;
 return (double)(size_t)(p);
}
