/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dse1-details" } */

void f(){
  char*p=__builtin_malloc(42);
  __builtin_memset(p,3,10);
  __builtin_memset(p,7,33);
}
char*g;
void h(){
  char*p=__builtin_malloc(42);
  g=__builtin_memset(p,3,10);
  __builtin_free(p);
}
char*i(){
  char*p=__builtin_malloc(42);
  __builtin_memset(p,3,10);
  __builtin_memset(p,7,33);
  return p;
}

/* { dg-final { scan-tree-dump-times "Deleted dead call" 4 "dse1" } } */
