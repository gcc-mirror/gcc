/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

typedef unsigned int uint_hi_t __attribute__ ((__mode__(__HI__)));

/* UPC runtime remote access prototype */
extern uint_hi_t __gethi2 (upc_shared_ptr_t);

relaxed shared uint_hi_t x;

uint_hi_t p () {
  return x;
}

/* { dg-final { scan-tree-dump-times "gethi2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
