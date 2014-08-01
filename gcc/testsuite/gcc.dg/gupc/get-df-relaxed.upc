/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


/* UPC runtime remote access prototype */
extern double __getdf2 (upc_shared_ptr_t);

relaxed shared double x;

double p () {
  return x;
}

/* { dg-final { scan-tree-dump-times "getdf2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
