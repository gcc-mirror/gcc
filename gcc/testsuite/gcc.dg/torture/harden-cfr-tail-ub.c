/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-returning-calls -fno-hardcfr-check-exceptions -fdump-tree-hardcfr -ffat-lto-objects -Wno-return-type" } */

/* In C only, check some additional cases (comparing with
   c-c++-common/torture/harden-cfr-tail.c) of falling off the end of non-void
   function.  C++ would issue an unreachable call in these cases.  */

extern int g (int i);

int f1(int i) {
  /* Inline check before the returning call, that doesn't return anything.  */
  g (i);
  /* Implicit return without value, despite the return type; this combination
     enables tail-calling of g, and is recognized as a returning call.  */
}

extern void g2 (int i);

int f2(int i) {
  /* Inline check before the returning call, that disregards its return
     value.  */
  g2 (i);
  /* Implicit return without value, despite the return type; this combination
     enables tail-calling of g2, and is recognized as a returning call.  */
}

int f3(int i) {
  if (i)
    /* Out-of-line check before the returning call.  */
    return g (i);
  /* Out-of-line check before implicit return.  */
}

/* Out-of-line checks in f3, before returning calls and before return.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 2 "hardcfr" } } */
/* Inline checking in all other functions.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "hardcfr" } } */
/* Check before tail-call in all functions, but f3 is out-of-line.  */
/* { dg-final { scan-tree-dump-times "Inserting inline check before stmt" 2 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "Inserting out-of-line check before stmt" 1 "hardcfr" } } */
