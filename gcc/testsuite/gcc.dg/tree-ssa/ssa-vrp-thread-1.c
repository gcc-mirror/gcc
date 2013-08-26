/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */


struct basic_block_def;
typedef struct basic_block_def *basic_block;
enum gimple_code
{
  LAST_AND_UNUSED_GIMPLE_CODE
};
struct omp_region
{
  struct omp_region *outer;
  basic_block cont;
};
void
build_omp_regions_1 (basic_block bb, struct omp_region *parent,
		     unsigned char single_tree, enum gimple_code code)
{
  if (code == 25)
    parent = parent->outer;
  else if (code == 42)
    parent->cont = bb;
  if (single_tree && !parent)
    return;
  oof ();
}

/* { dg-final { scan-tree-dump-times "Threaded" 1 "vrp1" }  } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

