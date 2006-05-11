/* Verify that constant equivalences get reloaded properly, either by being
   spilled to the stack, or regenerated, but not dropped to memory.  */
/* { dg-do compile { target { { i?86-*-* rs6000-*-* alpha*-*-* x86_64-*-* } || { powerpc*-*-* && ilp32 } } } } */
/* { dg-options "-O2 -fpic -fno-omit-frame-pointer -fno-asynchronous-unwind-tables" } */
/* { dg-final { scan-assembler-not "LC\[0-9\]" { xfail powerpc*-*-* } } } */

/* Clobber all call-saved registers that can hold a pointer value.  */
#if defined(__i386__)
#define clobber \
  asm volatile("#asm" : : : "si", "di")
#elif defined(__powerpc__) || defined(__PPC__) || defined(__ppc__) || defined(__POWERPC__) || defined(PPC) || defined (_IBMR2)
#define clobber \
  asm volatile("#asm" : : : "14", "15", "16", "17", "18", "19", "20", \
	       "21", "22", "23", "24", "25", "26", "27", "28", "29")
#elif defined(__alpha__)
#define clobber \
  asm volatile("#asm" : : : "$9", "$10", "$11", "$12", "$13", "$14", \
	       "$f2", "$f3", "$f4", "$f5", "$f6", "$f7", "$f8", "$f9")
#elif defined(__x86_64__)
#define clobber \
  asm volatile("#asm" : : : "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11")
#else
#error no clobber macro defined
#endif

extern void f1(int, int, int);
extern void f2(int*, int*, int*);

extern int ext;
static int loc_small;
static int loc_big[100];

void bar(void)
{
  f1(ext, loc_small, loc_big[0]);
  clobber;
  f2(&ext, &loc_small, &loc_big[0]);
}
