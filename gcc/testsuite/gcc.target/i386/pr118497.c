/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -fpic" } */
extern void crosscall2 (void (*fn) (void *, int), void *, int);
extern void _cgo_panic (void *, int);
extern void _cgo_allocate (void *, int);

void
callPanic (void)
{
  struct { const char *p; } a;
  a.p = "panic from C";
  crosscall2 (_cgo_panic, &a, sizeof a);
  *(int*) 1 = 1;
}

/* { dg-final { scan-assembler "__x86.get_pc_thunk.bx" } } */
