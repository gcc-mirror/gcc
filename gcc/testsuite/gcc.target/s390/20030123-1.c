/* This used to ICE due to a reload bug on s390*.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

extern void *alloca (__SIZE_TYPE__);

void func (char *p);

void test (void)
{
   char *p = alloca (4096);
   long idx;

   asm ("" : "=r" (idx) : : "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "12");

   func (p + idx + 1);
}

