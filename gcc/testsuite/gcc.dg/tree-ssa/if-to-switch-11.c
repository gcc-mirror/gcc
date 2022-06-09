/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized" } */

struct f {
  int len;
  int arr[4];
};

int
test (struct f const *const f)
{
  if (f->arr[3] == 1) {
    return 12;
  } else if (f->arr[3] == 2) {
    return 27;
  } else if (f->arr[3] == 3) {
    return 38;
  } else if (f->arr[3] == 4) {
    return 18;
  } else if (f->arr[3] == 5) {
    return 58;
  } else if (f->arr[3] == 6) {
    return 68;
  }
  return 0;
}

/* { dg-final { scan-tree-dump "Canonical GIMPLE case clusters: 1 2 3 4 5 6" "iftoswitch" } } */
