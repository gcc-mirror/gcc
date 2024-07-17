/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alias_ptx } } */
/* { dg-options "-save-temps" } */
/* { dg-add-options nvptx_alias_ptx } */

/* Copy of alias-1.c, with static __f and f.  */

int v;

static void __f ()
{
  v = 1;
}

static void f () __attribute__ ((alias ("__f")));

int
main (void)
{
  if (v != 0)
    __builtin_abort ();
  f ();
  if (v != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DECL: __f$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.func __f;$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DEF: __f$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.func __f$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DECL: f$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.func f;$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN FUNCTION DEF: f$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.alias f,__f;$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)\tcall __f;$} 0 } }
   { dg-final { scan-assembler-times {(?n)\tcall f;$} 1 } } */
