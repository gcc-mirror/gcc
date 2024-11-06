// { dg-do compile }
// { dg-options "-O2 -fstrict-enums --param case-values-threshold=1"}

typedef int basic_block;

enum gimple_code {};

struct omp_region {
  omp_region *outer;
  basic_block cont;
};

void
oof (void);

void
build_omp_regions_1 (omp_region *parent, basic_block bb, gimple_code code)
{
  if (code == 2)
    parent = parent->outer;
  else if (code != 0)
    parent->cont = bb;

  if (parent)
    oof ();
}
