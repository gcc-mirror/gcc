/* { dg-do compile } */
const int i = 0;

void f(void)
{
  __asm__ __volatile__ ("" : "=m" (i)); /* { dg-error "read-only variable" } */

}

void g(const int set)
{
  __asm__ __volatile__ ("" : "=r" (set)); /* { dg-error "read-only parameter" } */
}


