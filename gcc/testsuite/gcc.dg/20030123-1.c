/* This used to ICE due to a reload bug on s390*.  */

/* { dg-do compile { target s390*-*-* } } */
/* { dg-options "-O2" } */

void func (char *p);

void test (void)
{
   char *p = alloca (4096);
   long idx;

   asm ("" : "=r" (idx) : : "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12");

   func (p + idx + 1);
}

