/* { dg-do compile } */
/* { dg-options "-O3 -fno-inline-functions-called-once -fno-inline-small-functions -fno-tree-dce -fno-tree-forwprop -fno-tree-fre" } */

#if __SIZEOF_INT__ < 4
#define int __INT32_TYPE__
#endif

int h(void);
void l(int);
void func_56(int p_57, unsigned p_58) {
 // p_57 = 0x101BC642L;
  if (p_57 || h()) {
    int *l_105[2];
    l_105[0] = &p_57;
    l(p_57);
  }
}
void func_31(int p_33) {
  func_56(0x101BC642L, (p_33));
}
