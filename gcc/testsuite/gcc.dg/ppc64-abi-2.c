/* { dg-do run { target powerpc64-*-linux* } } */
/* { dg-options "-O2 -fprofile -mprofile-kernel -maltivec -mabi=altivec" } */
#include <stdarg.h>
#include <signal.h>
#include <altivec.h>

/* Testcase to check for ABI compliance of parameter passing
   for the PowerPC64 ABI.  */

void __attribute__((no_instrument_function))
sig_ill_handler (int sig)
{
    exit(0);
}

extern void abort (void);

typedef struct
{
  unsigned long gprs[8];
  double fprs[13];
  long pad;
  vector int vrs[12];
} reg_parms_t;

reg_parms_t gparms;

/* _mcount call is done on Linux ppc64 early in the prologue.
   my_mcount will provide a entry point _mcount,
   which will save all register to gparms. 
   Note that _mcount need to restore lr to original value,
   therefor use ctr to return.  
*/

void __attribute__((no_instrument_function))
my_mcount() 
{
  asm volatile (".type	_mcount,@function\n\t"
		".globl	_mcount\n\t"
		"_mcount:\n\t"
                "mflr 0\n\t"
                "mtctr 0\n\t"
                "ld 0,16(1)\n\t"
		"mtlr 0\n\t"
		"ld 11,gparms@got(2)\n\t" 
		"std 3,0(11)\n\t"		
		"std 4,8(11)\n\t"
		"std 5,16(11)\n\t"	
		"std 6,24(11)\n\t"	
		"std 7,32(11)\n\t"	
		"std 8,40(11)\n\t"	
		"std 9,48(11)\n\t"	
		"std 10,56(11)\n\t"   
		"stfd 1,64(11)\n\t"	
		"stfd 2,72(11)\n\t"	
		"stfd 3,80(11)\n\t"	
		"stfd 4,88(11)\n\t"	
		"stfd 5,96(11)\n\t"	
		"stfd 6,104(11)\n\t"	
		"stfd 7,112(11)\n\t"	
		"stfd 8,120(11)\n\t"	
		"stfd 9,128(11)\n\t"	
		"stfd 10,136(11)\n\t"	
		"stfd 11,144(11)\n\t"	
		"stfd 12,152(11)\n\t" 
		"stfd 13,160(11)\n\t" 
		"li 3,176\n\t"        
		"stvx 2,3,11\n\t"	
		"addi 3,3,16\n\t" 
		"stvx 3,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 4,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 5,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 6,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 7,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 8,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 9,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 10,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 11,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 12,3,11\n\t"	
		"addi 3,3,16\n\t"     
		"stvx 13,3,11\n\t"	
		"ld 3,0(11)\n\t"
		"bctr");
}

/* Stackframe structure relevant for parameter passing.  */
typedef union
{
  double d;
  unsigned long l;
  unsigned int i[2];
} parm_t;

typedef struct sf
{
  struct sf *backchain;
  long a1;
  long a2;
  long a3;
  long a4;
  long a5;
  parm_t slot[100];
} stack_frame_t;

typedef union
{
  unsigned int i[4];
  unsigned long l[2];
  vector int v;
} vector_int_t;

/* Paramter passing.
   s : gpr 3
   v : vpr 2
   i : gpr 7
*/
void __attribute__ ((noinline))
fcvi (char *s, vector int v, int i)
{
  reg_parms_t lparms = gparms;

  if (s != (char *) lparms.gprs[0])
    abort();

  if (!vec_all_eq (v, lparms.vrs[0]))
    abort ();

  if ((long) i != lparms.gprs[4])
    abort();
}
/* Paramter passing.
   s : gpr 3
   v : vpr 2
   w : vpr 3
*/

void __attribute__ ((noinline))
fcvv (char *s, vector int v, vector int w)
{
  vector int a, c = {6, 8, 10, 12};
  reg_parms_t lparms = gparms;

  if (s != (char *) lparms.gprs[0])
    abort();

  if (!vec_all_eq (v, lparms.vrs[0]))
    abort ();

  if (!vec_all_eq (w, lparms.vrs[1]))
    abort ();

  a = vec_add (v,w);

  if (!vec_all_eq (a, c))
    abort ();
}

/* Paramter passing.
   s : gpr 3
   i : gpr 4
   v : vpr 2
   w : vpr 3
*/
void __attribute__ ((noinline))
fcivv (char *s, int i, vector int v, vector int w)
{
  vector int a, c = {6, 8, 10, 12};
  reg_parms_t lparms = gparms;

  if (s != (char *) lparms.gprs[0])
    abort();

  if ((long) i != lparms.gprs[1])
    abort();

  if (!vec_all_eq (v, lparms.vrs[0]))
    abort ();

  if (!vec_all_eq (w, lparms.vrs[1]))
    abort ();

  a = vec_add (v,w);
  
  if (!vec_all_eq (a, c))
    abort ();
}

/* Paramter passing.
   s : gpr 3
   v : slot 2-3
   w : slot 4-5
*/

void __attribute__ ((noinline))
fcevv (char *s, ...)
{
  vector int a, c = {6, 8, 10, 12};
  vector int v,w;
  stack_frame_t *sp;
  reg_parms_t lparms = gparms;
  va_list arg;

  va_start (arg, s);

  if (s != (char *) lparms.gprs[0])
    abort();

  v = va_arg(arg, vector int);
  w = va_arg(arg, vector int);
  a = vec_add (v,w);
  
  if (!vec_all_eq (a, c))
    abort ();

  /* Go back one frame.  */
  sp = __builtin_frame_address(0);
  sp = sp->backchain;
  
  if (sp->slot[2].l != 0x100000002ULL
      || sp->slot[4].l != 0x500000006ULL)
    abort();
}

/* Paramter passing.
   s : gpr 3
   i : gpr 4
   j : gpr 5
   v : slot 4-5
   w : slot 6-7
*/
void __attribute__ ((noinline))
fciievv (char *s, int i, int j, ...)
{
  vector int a, c = {6, 8, 10, 12};
  vector int v,w;
  stack_frame_t *sp;
  reg_parms_t lparms = gparms;
  va_list arg;

  va_start (arg, j);

  if (s != (char *) lparms.gprs[0])
    abort();

  if ((long) i != lparms.gprs[1])
    abort();

  if ((long) j != lparms.gprs[2])
    abort();
  
  v = va_arg(arg, vector int);
  w = va_arg(arg, vector int);
  a = vec_add (v,w);
  
  if (!vec_all_eq (a, c))
    abort ();

  sp = __builtin_frame_address(0);
  sp = sp->backchain;
  
  if (sp->slot[4].l != 0x100000002ULL
      || sp->slot[6].l != 0x500000006ULL)
    abort();
}

void __attribute__ ((noinline))
fcvevv (char *s, vector int x, ...)
{
  vector int a, c = {7, 10, 13, 16};
  vector int v,w;
  stack_frame_t *sp;
  reg_parms_t lparms = gparms;
  va_list arg;

  va_start (arg, x);
  
  v = va_arg(arg, vector int);
  w = va_arg(arg, vector int);

  a = vec_add (v,w);
  a = vec_add (a, x);
 
  if (!vec_all_eq (a, c))
    abort ();

  sp = __builtin_frame_address(0);
  sp = sp->backchain;
  
  if (sp->slot[4].l != 0x100000002ULL
      || sp->slot[6].l != 0x500000006ULL)
    abort();
}

void fnp_cvvvv();

int __attribute__((no_instrument_function, noinline))
main1() 
{   
  char *s = "vv";
  vector int v = {1, 2, 3, 4};
  vector int w = {5, 6, 7, 8};
 
  fcvi (s, v, 2);
  fcvv (s, v, w);
  fnp_cvvvv (s, v, w, v, w);
  fcivv (s, 1, v, w);
  fcevv (s, v, w);
  fciievv (s, 1, 2, v, w);
  fcvevv (s, v, v, w);
  return 0;
}

int __attribute__((no_instrument_function)) 
main()
{
  /* Exit on systems without altivec.  */
  signal (SIGILL, sig_ill_handler);
  /* Altivec instruction, 'vor %v0,%v0,%v0'.  */
  asm volatile (".long 0x10000484");
  signal (SIGILL, SIG_DFL);

  return main1 ();
}

/* Paramter passing.
   Function called with no prototype.
   s : gpr 3
   v : vpr 2 gpr 5-6
   w : vpr 3 gpr 7-8
   x : vpr 4 gpr 9-10
   y : vpr 5 slot 8-9
*/
void
fnp_cvvvv (char *s, vector int v, vector int w,
	   vector int x, vector int y)
{
  vector int a, c = {12, 16, 20, 24};
  reg_parms_t lparms = gparms;
  stack_frame_t *sp;
  vector_int_t v0, v1, v2, v3;

  if (s != (char *) lparms.gprs[0])
    abort();

  if (!vec_all_eq (v, lparms.vrs[0]))
    abort ();

  if (!vec_all_eq (w, lparms.vrs[1]))
    abort ();

  if (!vec_all_eq (x, lparms.vrs[2]))
    abort ();

  if (!vec_all_eq (y, lparms.vrs[3]))
    abort ();

  a = vec_add (v,w);
  a = vec_add (a,x);  
  a = vec_add (a,y);  
  
  if (!vec_all_eq (a, c))
    abort ();
  
  v0.v = lparms.vrs[0];
  v1.v = lparms.vrs[1];
  v2.v = lparms.vrs[2];
  v3.v = lparms.vrs[3];

  if (v0.l[0] != lparms.gprs[2])
    abort ();

  if (v0.l[1] != lparms.gprs[3])
    abort ();

  if (v1.l[0] != lparms.gprs[4])
    abort ();

  if (v1.l[1] != lparms.gprs[5])
    abort ();

  if (v2.l[0] != lparms.gprs[6])
    abort ();

  if (v2.l[1] != lparms.gprs[7])
    abort ();

  sp = __builtin_frame_address(0);
  sp = sp->backchain;
  
  if (sp->slot[8].l != v3.l[0])
    abort ();

  if (sp->slot[9].l != v3.l[1])
    abort ();
}  
  
