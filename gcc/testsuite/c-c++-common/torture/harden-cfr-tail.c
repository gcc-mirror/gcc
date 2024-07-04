/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-returning-calls -fno-hardcfr-check-exceptions -fdump-tree-hardcfr -ffat-lto-objects -Wno-return-type" } */

/* Check that we insert CFR checking so as to not disrupt tail calls.
   Mandatory tail calls are not available in C, and optimizing calls as tail
   calls only takes place after hardcfr, so we insert checking before calls
   followed by copies and return stmts with the same return value, that might
   (or might not) end up optimized to tail calls.  */

extern int g (int i);

int f1(int i) {
  /* Inline check before the returning call.  */
  return g (i);
}

extern void g2 (int i);

void f2(int i) {
  /* Inline check before the returning call, that ignores the returned value,
     matching the value-less return.  */
  g2 (i);
  return;
}

void f3(int i) {
  /* Inline check before the returning call.  */
  g (i);
}

void f4(int i) {
  if (i)
    /* Out-of-line check before the returning call.  */
    return g2 (i);
  /* Out-of-line check before implicit return.  */
}

int f5(int i) {
  /* Not regarded as a returning call, returning value other than callee's
     returned value.  */
  g (i);
  /* Inline check after the non-returning call.  */
  return i;
}

/* Out-of-line checks in f4, before returning calls and before return.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 2 "hardcfr" } } */
/* Inline checking in all other functions.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 4 "hardcfr" } } */
/* Check before tail-call in all but f5, but f4 is out-of-line.  */
/* { dg-final { scan-tree-dump-times "Inserting inline check before stmt" 3 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "Inserting out-of-line check before stmt" 1 "hardcfr" } } */
