// { dg-do compile }
// { dg-options "-O2 -fno-tree-forwprop" }

void *a, *b, *c;
void foo(void) {
  c = (void *)((__INTPTR_TYPE__)a & (__INTPTR_TYPE__)b);
}
