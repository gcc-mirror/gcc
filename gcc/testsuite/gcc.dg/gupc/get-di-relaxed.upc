/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

typedef unsigned int uint_di_t __attribute__ ((__mode__(__DI__)));

/* UPC runtime remote access prototype */
extern uint_di_t __getdi2 (upc_shared_ptr_t);

relaxed shared uint_di_t x;

uint_di_t p () {
  return x;
}

/* { dg-final { scan-tree-dump-times "getdi2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
