/* { dg-do compile } */
/* { dg-options "-fstack-protector -O3 -std=gnu17" } */

/* PR78695: This code used to ICE in rs6000.c:find_alignment_op because
   the stack protector address definition isn't associated with an insn.  */

void *a();
long b() {
  char c[1];
  char *d = a(), *e = c;
  long f = e ? b(e) : 0;
  if (f > 54)
    f = 1;
  while (f--)
    *d++ = *e++;
}
