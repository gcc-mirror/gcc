/* PR 11527 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

typedef struct smrdd_memory_blocks_s
{
  int blocks;
  int block[];
} smrdd_memory_blocks_t;

const smrdd_memory_blocks_t smrdd_memory_blocks =
{
  3,
  {
    [5] = 5,
    [1] = 2,
  }
};
