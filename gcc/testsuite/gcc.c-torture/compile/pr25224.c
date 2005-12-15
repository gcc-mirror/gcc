/* { dg-options "-funswitch-loops" } */
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
