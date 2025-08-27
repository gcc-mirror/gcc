/* { dg-do compile } */
/* PR tree-optimization/121695 */

int ac;
char p;
int *r;
static unsigned t = 7;
int q() {
  int v;
af: {
  int ag[3];
  int *ah = &ag[1];
  for (; ac;) {
    int ai = 3971866093;
    if (0 >= *ah && (*r = 1))
      *ah &= ai;
    else {
      if (p)
        goto af;
      *ah &= t;
    }
  }
}
  return v;
}
