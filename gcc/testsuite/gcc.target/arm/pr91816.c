/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-additional-options "-mthumb" }  */
/* { dg-timeout-factor 4.0 } */

int printf(const char *, ...);

#define HW0	printf("Hello World!\n");
#define HW1	HW0 HW0 HW0 HW0 HW0 HW0 HW0 HW0 HW0 HW0
#define HW2	HW1 HW1 HW1 HW1 HW1 HW1 HW1 HW1 HW1 HW1
#define HW3	HW2 HW2 HW2 HW2 HW2 HW2 HW2 HW2 HW2 HW2
#define HW4	HW3 HW3 HW3 HW3 HW3 HW3 HW3 HW3 HW3 HW3
#define HW5	HW4 HW4 HW4 HW4 HW4 HW4 HW4 HW4 HW4 HW4
#define HW6	HW5 HW5

__attribute__((noinline,noclone)) void f1 (int a)
{
  if (a) { HW0 }
}

__attribute__((noinline,noclone)) void f2 (int a)
{
  if (a) { HW3 }
}


__attribute__((noinline,noclone)) void f3 (int a)
{
  if (a) { HW6 }
}

__attribute__((noinline,noclone)) void f4 (int a)
{
  if (a == 1) { HW0 }
}

__attribute__((noinline,noclone)) void f5 (int a)
{
  if (a == 1) { HW3 }
}


__attribute__((noinline,noclone)) void f6 (int a)
{
  if (a == 1) { HW6 }
}


int main(void)
{
	f1(0);
	f2(0);
	f3(0);
	f4(0);
	f5(0);
	f6(0);
	return 0;
}


/* { dg-final { scan-assembler-times "beq\\t.L\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "beq\\t.Lbcond\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "bne\\t.L\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "bne\\t.Lbcond\[0-9\]" 1 } } */
