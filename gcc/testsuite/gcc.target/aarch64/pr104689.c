/* PR target/104689. Unwind across pac-ret frames with unusual dwarf.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-fexceptions -O2" } */

#include <unwind.h>
#include <stdlib.h>
#include <stdio.h>

#define die() \
  do { \
    printf ("%s:%d: reached unexpectedly.\n", __FILE__, __LINE__); \
    fflush (stdout); \
    abort (); \
  } while (0)


/* Code to invoke unwinding with a logging callback.  */

static struct _Unwind_Exception exc;

static _Unwind_Reason_Code
force_unwind_stop (int version, _Unwind_Action actions,
                   _Unwind_Exception_Class exc_class,
                   struct _Unwind_Exception *exc_obj,
                   struct _Unwind_Context *context,
                   void *stop_parameter)
{
  printf ("%s: CFA: %p PC: %p actions: %d\n",
	  __func__,
	  (void *)_Unwind_GetCFA (context),
	  (void *)_Unwind_GetIP (context),
	  (int)actions);
  if (actions & _UA_END_OF_STACK)
    die ();
  return _URC_NO_REASON;
}

static void force_unwind (void)
{
#ifndef __USING_SJLJ_EXCEPTIONS__
  _Unwind_ForcedUnwind (&exc, force_unwind_stop, 0);
#else
  _Unwind_SjLj_ForcedUnwind (&exc, force_unwind_stop, 0);
#endif
}


/* Define functions with unusual pac-ret dwarf via top level asm.  */

#define STR(x) #x
#define DW_CFA_val_expression 0x16
#define RA_SIGN_STATE 34
#define DW_OP_lit0 0x30
#define DW_OP_lit1 0x31

#define cfi_escape(a1, a2, a3, a4) \
  ".cfi_escape " STR(a1) ", " STR(a2) ", " STR(a3) ", " STR(a4)

/* Bytes: 0x16 0x22 0x01 0x30  */
#define SET_RA_STATE_0 \
  cfi_escape (DW_CFA_val_expression, RA_SIGN_STATE, 1, DW_OP_lit0)

/* Bytes: 0x16 0x22 0x01 0x31  */
#define SET_RA_STATE_1 \
  cfi_escape (DW_CFA_val_expression, RA_SIGN_STATE, 1, DW_OP_lit1)

/* These function call their argument.  */
void unusual_pac_ret (void *);
void unusual_no_pac_ret (void *);

asm(""
".global unusual_pac_ret\n"
".type unusual_pac_ret, %function\n"
"unusual_pac_ret:\n"
"	.cfi_startproc\n"
"	" SET_RA_STATE_0 "\n"
"	hint	25 // paciasp\n"
"	" SET_RA_STATE_1 "\n"
"	stp	x29, x30, [sp, -16]!\n"
"	.cfi_def_cfa_offset 16\n"
"	.cfi_offset 29, -16\n"
"	.cfi_offset 30, -8\n"
"	mov	x29, sp\n"
"	blr	x0\n"
"	ldp	x29, x30, [sp], 16\n"
"	.cfi_restore 30\n"
"	.cfi_restore 29\n"
"	.cfi_def_cfa_offset 0\n"
"	hint	29 // autiasp\n"
"	" SET_RA_STATE_0 "\n"
"	ret\n"
"	.cfi_endproc\n");

asm(""
".global unusual_no_pac_ret\n"
".type unusual_no_pac_ret, %function\n"
"unusual_no_pac_ret:\n"
"	.cfi_startproc\n"
"	" SET_RA_STATE_0 "\n"
"	stp	x29, x30, [sp, -16]!\n"
"	.cfi_def_cfa_offset 16\n"
"	.cfi_offset 29, -16\n"
"	.cfi_offset 30, -8\n"
"	mov	x29, sp\n"
"	blr	x0\n"
"	ldp	x29, x30, [sp], 16\n"
"	.cfi_restore 30\n"
"	.cfi_restore 29\n"
"	.cfi_def_cfa_offset 0\n"
"	ret\n"
"	.cfi_endproc\n");


/* Functions to create a call chain with mixed pac-ret dwarf.  */

__attribute__((target("branch-protection=pac-ret")))
static void f2_pac_ret (void)
{
  force_unwind ();
  die ();
}

__attribute__((target("branch-protection=none")))
static void f1_no_pac_ret (void)
{
  unusual_pac_ret (f2_pac_ret);
  die ();
}

__attribute__((noinline, target("branch-protection=pac-ret")))
static void f0_pac_ret (void)
{
  unusual_no_pac_ret (f1_no_pac_ret);
  die ();
}

static void cleanup_handler (void *p)
{
  printf ("%s: Success.\n", __func__);
  exit (0);
}

int main ()
{
  char dummy __attribute__((cleanup (cleanup_handler)));
  f0_pac_ret ();
  die ();
}
