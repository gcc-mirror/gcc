/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */
/* { dg-require-effective-target longdouble128 } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];


/* UPC runtime remote access prototype */
extern void __putstf2 (upc_shared_ptr_t, long double);
extern void __putsxf2 (upc_shared_ptr_t, long double);

strict shared long double x;

void p () {
  x = 1;
}

/* { dg-final { scan-tree-dump-times "puts\[tx\]f2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
