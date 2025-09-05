/* { dg-do compile } */
/* PR tree-optimization/121695 */

__INT32_TYPE__ ac;
char p;
__INT32_TYPE__ *r;
static __UINT32_TYPE__ t = 7;
__INT32_TYPE__ q() {
  __INT32_TYPE__ v;
af: {
  __INT32_TYPE__ ag[3];
  __INT32_TYPE__ *ah = &ag[1];
  for (; ac;) {
    __INT32_TYPE__ ai = 3971866093;
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
