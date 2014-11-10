/* { dg-options "-funswitch-loops" } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

static float rgam;
extern void *jmp(void *);

void drotmg(float d1) {
void *labels[] = { &&L170, &&L180, 0 };

  for(;;) {
    goto *jmp(labels);
    if (d1 <= rgam)
      goto L170;

L170:
    if (d1 <= rgam)
      goto L170;
  }

L180:
  goto L170;
}
