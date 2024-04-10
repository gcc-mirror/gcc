/* { dg-do compile } */
/* { dg-options "-funroll-loops" } */
float val[128];
float x;
void bar() {
  int i = 55;
  for (; i >= 0; --i)
    x += val[i];
}
