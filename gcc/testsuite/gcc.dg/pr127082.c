/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int src(int v0_i8) {
  if (!((-96 <= v0_i8) && (v0_i8 <= -65))) __builtin_unreachable();
  int i0_i8 = (int)96 % v0_i8;
  int i1_i8 = i0_i8 | -96;
  return i1_i8;
}

/* { dg-final { scan-tree-dump "0, 31" "evrp" } } */
