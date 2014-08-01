/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


/* UPC runtime remote access prototype */
extern void __putsf2 (upc_shared_ptr_t, float);

relaxed shared float x;

void p () {
  x = 1;
}

/* { dg-final { scan-tree-dump-times "putsf2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
