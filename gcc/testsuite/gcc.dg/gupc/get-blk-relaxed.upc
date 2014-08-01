/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

/* The base address of the UPC shared section */
extern char __upc_shared_start[1];

typedef struct { unsigned char c[16]; } blk_t;

/* UPC runtime remote access prototype */
typedef __SIZE_TYPE__ size_t;
extern blk_t __getblk3 (void *, upc_shared_ptr_t, size_t);

relaxed shared blk_t x;
blk_t local_x;

void p () {
  local_x = x;
}

/* { dg-final { scan-tree-dump-times "getblk3" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "get|put" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
