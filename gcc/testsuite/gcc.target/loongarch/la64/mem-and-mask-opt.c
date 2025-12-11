/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { scan-assembler-not "bstrpick" } } */
/* { dg-final { scan-assembler "ld\\.wu" } } */

struct st
{
  char const *name;
};
struct fst
{
  struct st *groups;
};

struct fst *pfunc (int);

const char *
test (int pc, unsigned group)
{
  struct fst *pci = pfunc (pc);

  return pci->groups[group].name;
}
