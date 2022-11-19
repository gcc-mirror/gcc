/* We used to ICE in the gimplifier, PR 106560. */
/* { dg-do compile } */
/* { dg-options "-w" } */
void **a; /* { dg-note "" } */
void b() { void **c = a; }
a; /* { dg-error "" } */
