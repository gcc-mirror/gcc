/* PR target/59363 */
/* { dg-do run } */
/* { dg-options "-O2 -mtune=amdfam10" } */

typedef struct {
  int ctxlen;
  long interhunkctxlen;
  int flags;
  long find_func;
  void *find_func_priv;
  int hunk_func;
} xdemitconf_t;

__attribute__((noinline))
int xdi_diff(xdemitconf_t *xecfg) {
  if (xecfg->hunk_func == 0)
    __builtin_abort();
  return 0;
}
int main() {
  xdemitconf_t xecfg = {0};
  xecfg.hunk_func = 1;
  return xdi_diff(&xecfg);
}
