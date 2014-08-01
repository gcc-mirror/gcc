/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

/* Runtime API */
extern void __upc_init_decls (void);
extern void __putsi2 (upc_shared_ptr_t, int);

shared int x = 10;

shared int *xp = &x;

int main () {
  return 0;
}
/* { dg-final { scan-tree-dump-times "Function __upc_init_decls" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "__putsi2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "xp = " 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
