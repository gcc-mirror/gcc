/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */
/* { dg-require-effective-target int128 } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

typedef unsigned int uint_ti_t __attribute__ ((__mode__(__TI__)));

/* UPC runtime remote access prototype */
typedef __SIZE_TYPE__ size_t;
extern void __putblk3 (upc_shared_ptr_t, void *, size_t);

relaxed shared uint_ti_t x;

void p () {
  x = 1;
}

/* { dg-final { scan-tree-dump-times "putblk3" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
