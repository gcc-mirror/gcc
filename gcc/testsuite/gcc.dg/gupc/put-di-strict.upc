/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

typedef unsigned int uint_di_t __attribute__ ((__mode__(__DI__)));

/* UPC runtime remote access prototype */
extern void __putsdi2 (upc_shared_ptr_t, uint_di_t);

strict shared uint_di_t x;

void p () {
  x = 1;
}

/* { dg-final { scan-tree-dump-times "putsdi2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
