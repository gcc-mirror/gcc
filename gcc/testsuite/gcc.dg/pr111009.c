/* PR tree-optimization/111009 */
/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-overflow" } */

struct dso {
 struct dso * next;
 int maj;
};

__attribute__((noipa)) static void __dso_id__cmp_(void) {}

__attribute__((noipa))
static int bug(struct dso * d, struct dso *dso)
{
 struct dso **p = &d;
 struct dso *curr = 0;

 while (*p) {
  curr = *p;
  // prevent null deref below
  if (!dso) return 1;
  if (dso == curr) return 1;

  int *a = &dso->maj;
  // null deref
  if (!(a && *a)) __dso_id__cmp_();

  p = &curr->next;
 }
 return 0;
}

__attribute__((noipa))
int main(void) {
    struct dso d = { 0, 0, };
    bug(&d, 0);
}

