/* { dg-do run { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { *-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2" } */

typedef __builtin_va_list va_list;
#define va_start(ap, arg) __builtin_va_start (ap, arg)
#define va_arg(ap, type)  __builtin_va_arg (ap, type)

/* Testcase to check for ABI compliance of parameter passing
   for the PowerPC64 ABI.  
   Parameter passing of integral and floating point is tested.  */

extern void abort (void);

typedef struct
{
  unsigned long gprs[8];
  double fprs[13];
} reg_parms_t;

volatile reg_parms_t gparms;


/* Testcase could break on future gcc's, if parameter regs are changed
   before this asm.  To minimize the risk of that happening the test
   consists of two sets of functions wih identical signatures:
   foo, which does nothing except save function argument registers
       to prevent them from getting clobbered (see PR65109),
   foo_check, which verifies that the values of function registers
       saved by foo match those passed to foo_check by the caller.  */

#ifndef __MACH__
#define save_parms()					 \
  asm volatile ("ld 11,gparms@got(2)\n\t"                \
                  "std 3,0(11)\n\t"                     \
                  "std 4,8(11)\n\t"                     \
                  "std 5,16(11)\n\t"                    \
                  "std 6,24(11)\n\t"                    \
                  "std 7,32(11)\n\t"                    \
                  "std 8,40(11)\n\t"                    \
                  "std 9,48(11)\n\t"                    \
                  "std 10,56(11)\n\t"                 \
                  "stfd 1,64(11)\n\t"                   \
                  "stfd 2,72(11)\n\t"                   \
                  "stfd 3,80(11)\n\t"                   \
                  "stfd 4,88(11)\n\t"                   \
                  "stfd 5,96(11)\n\t"                   \
                  "stfd 6,104(11)\n\t"                  \
                  "stfd 7,112(11)\n\t"                  \
                  "stfd 8,120(11)\n\t"                  \
                  "stfd 9,128(11)\n\t"                  \
                  "stfd 10,136(11)\n\t"                 \
                  "stfd 11,144(11)\n\t"                 \
                  "stfd 12,152(11)\n\t"                 \
                  "stfd 13,160(11)\n\t":::"11", "memory")
#else
#define save_parms()				      \
  asm volatile ("ld r11,gparms@got(r2)\n\t"           \
                  "std r3,0(r11)\n\t"                   \
                  "std r4,8(r11)\n\t"                   \
                  "std r5,16(r11)\n\t"                  \
                  "std r6,24(r11)\n\t"                  \
                  "std r7,32(r11)\n\t"                  \
                  "std r8,40(r11)\n\t"                  \
                  "std r9,48(r11)\n\t"                  \
                  "std r10,56(r11)\n\t"                 \
                  "stfd f1,64(r11)\n\t"                 \
                  "stfd f2,72(r11)\n\t"                 \
                  "stfd f3,80(r11)\n\t"                 \
                  "stfd f4,88(r11)\n\t"                 \
                  "stfd f5,96(r11)\n\t"                 \
                  "stfd f6,104(r11)\n\t"                \
                  "stfd f7,112(r11)\n\t"                \
                  "stfd f8,120(r11)\n\t"                \
                  "stfd f9,128(r11)\n\t"                \
                  "stfd f10,136(r11)\n\t"               \
                  "stfd f11,144(r11)\n\t"               \
                  "stfd f12,152(r11)\n\t"               \
		  "stfd f13,160(r11)\n\t":::"r11", "memory")
#endif


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
#if _CALL_ELF != 2
  long a4;
  long a5;
#endif
  parm_t slot[100];
} stack_frame_t;


/* Paramter passing.
   s : gpr 3
   l : gpr 4
   d : fpr 1
*/
void __attribute__ ((noinline)) fcld (char *s, long l, double d)
{
  save_parms ();

}
void __attribute__ ((noinline)) fcld_check (char *s, long l, double d)
{
  if (s != (char *) gparms.gprs[0])
    abort ();

  if (l != gparms.gprs[1])
    abort ();

  if (d != gparms.fprs[0])
    abort ();
}

/* Paramter passing.
   s : gpr 3
   l : gpr 4
   d : fpr 2
   i : gpr 5
*/
void __attribute__ ((noinline))
fcldi (char *s, long l, double d, signed int i)
{
  save_parms ();
}

void __attribute__ ((noinline))
fcldi_check (char *s, long l, double d, signed int i)
{
  if (s != (char *) gparms.gprs[0])
    abort ();

  if (l != gparms.gprs[1])
    abort ();

  if (d != gparms.fprs[0])
    abort ();

  if ((signed long) i != gparms.gprs[3])
    abort ();
}

/* Paramter passing.
   s : gpr 3
   l : gpr 4
   d : fpr 2
   i : gpr 5
*/
void __attribute__ ((noinline))
fcldu (char *s, long l, float d, unsigned int i)
{
  save_parms ();
}

void __attribute__ ((noinline))
fcldu_check (char *s, long l, float d, unsigned int i)
{
  if (s != (char *) gparms.gprs[0])
    abort ();

  if (l != gparms.gprs[1])
    abort ();

  if ((double) d != gparms.fprs[0])
    abort ();

  if ((unsigned long) i != gparms.gprs[3])
    abort ();
}

/* Paramter passing.
   s : gpr 3
   l : slot 1
   d : slot 2
*/
void __attribute__ ((noinline)) fceld (char *s, ...)
{
  save_parms ();
}

void __attribute__ ((noinline)) fceld_check (char *s, ...)
{
  stack_frame_t *sp;
  va_list arg;
  double d;
  long l;

  va_start (arg, s);

  if (s != (char *) gparms.gprs[0])
    abort ();

  l = va_arg (arg, long);
  d = va_arg (arg, double);

  /* Go back one frame.  */
  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (sp->slot[1].l != l)
    abort ();

  if (sp->slot[2].d != d)
    abort ();
}

/* Paramter passing.
   s : gpr 3
   i : gpr 4
   j : gpr 5
   d : slot 3
   l : slot 4
*/
void __attribute__ ((noinline)) fciiedl (char *s, int i, int j, ...)
{
  save_parms ();
}

void __attribute__ ((noinline)) fciiedl_check (char *s, int i, int j, ...)
{
  stack_frame_t *sp;
  va_list arg;
  double d;
  long l;

  va_start (arg, j);

  if (s != (char *) gparms.gprs[0])
    abort ();

  if ((long) i != gparms.gprs[1])
    abort ();

  if ((long) j != gparms.gprs[2])
    abort ();

  d = va_arg (arg, double);
  l = va_arg (arg, long);

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (sp->slot[3].d != d)
    abort ();

  if (sp->slot[4].l != l)
    abort ();
}

/* 
Parameter     Register     Offset in parameter save area
c             r3           0-7    (not stored in parameter save area)
ff            f1           8-15   (not stored)
d             r5           16-23  (not stored)
ld            f2           24-31  (not stored)
f             r7           32-39  (not stored)
s             r8,r9        40-55  (not stored)
gg            f3           56-63  (not stored)
t             (none)       64-79  (stored in parameter save area)
e             (none)       80-87  (stored)
hh            f4           88-95  (stored)  

*/

typedef struct
{
  int a;
  double dd;
} sparm;

typedef union
{
  int i[2];
  long l;
  double d;
} double_t;

/* Example from ABI documentation with slight changes.
   Paramter passing. 
   c  : gpr 3
   ff : fpr 1
   d  : gpr 5
   ld : fpr 2
   f  : gpr 7
   s  : gpr 8 - 9
   gg : fpr 3
   t  : save area offset 64 - 79 
   e  : save area offset 80 - 88
   hh : fpr 4   
*/

void __attribute__ ((noinline))
fididisdsid (int c, double ff, int d, double ld, int f,
	     sparm s, double gg, sparm t, int e, double hh)
{
  save_parms ();
}

void
fididisdsid_check (int c, double ff, int d, double ld, int f,
		   sparm s, double gg, sparm t, int e, double hh)
{
  stack_frame_t *sp;
  double_t dx, dy;

  /* Parm 0: int.  */
  if ((long) c != gparms.gprs[0])
    abort ();

  /* Parm 1: double.  */
  if (ff != gparms.fprs[0])
    abort ();

  /* Parm 2: int.  */
  if ((long) d != gparms.gprs[2])
    abort ();

  /* Parm 3: double.  */
  if (ld != gparms.fprs[1])
    abort ();

  /* Parm 4: int.  */
  if ((long) f != gparms.gprs[4])
    abort ();

  /* Parm 5: struct sparm.  */
  dx.l = gparms.gprs[5];
  dy.l = gparms.gprs[6];

  if (s.a != dx.i[0])
    abort ();
  if (s.dd != dy.d)
    abort ();

  /* Parm 6: double.  */
  if (gg != gparms.fprs[2])
    abort ();

  sp = ((stack_frame_t*)__builtin_frame_address (0))->backchain;

  /* Parm 7: struct sparm.  */
  dx.l = sp->slot[8].l;
  dy.l = sp->slot[9].l;
  if (t.a != dx.i[0])
    abort ();
  if (t.dd != dy.d)
    abort ();

  /* Parm 8: int.  */
  if (e != sp->slot[10].l)
    abort ();

  /* Parm 9: double.  */

  if (hh != gparms.fprs[3])
    abort ();
}

int
main ()
{
  char *s = "ii";

#define ABI_CHECK(func, args) \
  func args, func ## _check args

  ABI_CHECK (fcld, (s, 1, 1.0));
  ABI_CHECK (fcldi, (s, 1, 1.0, -2));
  ABI_CHECK (fcldu, (s, 1, 1.0, 2));
  ABI_CHECK (fceld, (s, 1, 1.0));
  ABI_CHECK (fciiedl, (s, 1, 2, 1.0, 3));
  ABI_CHECK (fididisdsid, (1, 1.0, 2, 2.0, -1,
			   (sparm){3, 3.0}, 4.0, (sparm){5, 5.0},
			   6, 7.0));

  return 0;
}
