/* { dg-options "-fno-tree-dse -Ofast -fno-tree-coalesce-vars -fno-dce -fno-tree-dce" } */

struct TV4 {
  __attribute__((vector_size(sizeof(int) * 4))) int v;
};
void modify() {
  struct TV4 __trans_tmp_1, temp;
  temp.v[0] = temp.v[3] = 0;
  __trans_tmp_1 = temp;
}
