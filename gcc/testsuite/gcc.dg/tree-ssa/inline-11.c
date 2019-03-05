/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "-O2 -fdump-tree-einline-all" } */
int w;
int bar (void) __attribute__ ((weak));
int bar (){
  w++;
}
void foo()
{
  bar();
}
/* { dg-final { scan-tree-dump-times "function body can be overwritten at link time" 1 "einline" } } */
