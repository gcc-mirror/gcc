/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

typedef unsigned int uint_si_t __attribute__ ((__mode__(__SI__)));

/* UPC runtime remote access prototype */
extern uint_si_t __getssi2 (upc_shared_ptr_t);

strict shared uint_si_t x;

uint_si_t p () {
  return x;
}

/* { dg-final { scan-tree-dump-times "getssi2" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
