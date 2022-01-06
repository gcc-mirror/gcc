/* PR c++/83059 - ICE on invalid C++ code: in tree_to_uhwi, at tree.c:6633 */
/* { dg-do compile } */

void
foo (int *p, int *q, int *r)
{
  __atomic_compare_exchange (p, q, r, 0, 0, -1);	/* { dg-warning "invalid memory model argument 6" } */
}

/* The test triggers several distinct instance of the warning.  Prune
   them out; they're not relevant to its main purpose -- to verify
   there's no ICE.
   { dg-prune-output "-Winvalid-memory-model" } */
