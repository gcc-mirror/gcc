/* Test asm const. */
/* { dg-do compile } */
/* { dg-final { scan-assembler-times "foo:.*\\.long 1061109567.*\\.long 52" 1 } } */
int foo ()
{
  __asm__ volatile (
          "foo:"
          "\n\t"
	  ".long %a0\n\t"
	  ".long %a1\n\t"
	  :
	  :"i"(0x3f3f3f3f), "i"(52)
	  :
	  );
}

