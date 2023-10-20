/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-returning-calls -fno-exceptions -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we insert checks before returning calls and alternate paths, even
   at -O0, because of the explicit command-line flag.  */

void g (void);
void g2 (void);
void g3 (void);

void f (int i) {
  if (!i)
    /* Out-of-line checks here...  */
    g ();
  else if (i > 0)
    /* here...  */
    g2 ();
  /* else */
    /* and in the implicit else here.  */
}

void f2 (int i) {
  if (!i)
    /* Out-of-line check here...  */
    g ();
  else if (i > 0)
    /* here...  */
    g2 ();
  else
    /* and here.  */
    g3 ();
}

/* { dg-final { scan-tree-dump-times "hardcfr_check" 6 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 0 "hardcfr" } } */
