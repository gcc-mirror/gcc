/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* Runtime API */
extern void __upc_barrier (int);
extern void __upc_notify (int);
extern void __upc_wait (int);

int main () {
  /* { dg-final { scan-tree-dump-times "__upc_barrier \\\(123\\\)" 1 "original" } } */
  upc_barrier 123;
  /* { dg-final { scan-tree-dump-times "__upc_notify \\\(456\\\)" 1 "original" } } */
  upc_notify 456;
  /* { dg-final { scan-tree-dump-times "__upc_wait \\\(456\\\)" 1 "original" } } */
  upc_wait 456;
  return 0;
}
/* { dg-final { cleanup-tree-dump "original" } } */
