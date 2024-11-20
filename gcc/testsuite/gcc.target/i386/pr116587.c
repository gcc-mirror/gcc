/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mstackrealign -mavx512f -fharden-control-flow-redundancy -fno-omit-frame-pointer -mbmi -fkeep-gc-roots-live" } */

typedef __UINT64_TYPE__ a;
int b;
struct c {
  a d;
};
extern char e[];
int f;
void g();
char *h(struct c *i, a d) {
  while (b) {
    if ((i->d & d) == i->d) {
      if (f)
        g();
      g();
      d &= ~i->d;
    }
    ++i;
  }
  if (d)
    g();
  if (f)
    return "";
  return e;
}
