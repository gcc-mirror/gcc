/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* Runtime API */
extern void * __getaddr (upc_shared_ptr_t);

shared int *p;

int main () {
  int *local_p;
  /* { dg-final { scan-tree-dump-times "local_p = .*__getaddr.* p\\\)" 1 "original" } } */
  local_p = (void *)p;
  return 0;
}
/* { dg-final { cleanup-tree-dump "original" } } */
