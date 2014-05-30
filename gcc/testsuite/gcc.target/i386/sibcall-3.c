/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2" } */

extern 
#ifdef _WIN32
 __declspec (dllimport)
#endif
 void foo (int a);

void bar (int a)
{
  return foo (a);
}

/* { dg-final { scan-assembler-not "jmp[ \t]*.%eax" } } */
