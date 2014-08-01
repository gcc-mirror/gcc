/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


/* UPC runtime remote access prototype */
extern void __putdf2 (upc_shared_ptr_t, double);

relaxed shared double x;

void p () {
  x = 1;
}

/* { dg-final { scan-tree-dump-times "putdf2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
