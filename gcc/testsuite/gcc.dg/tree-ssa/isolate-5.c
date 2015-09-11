/* { dg-do compile } */ 
/* { dg-options "-O2 -fdelete-null-pointer-checks -fdump-tree-isolate-paths -fdump-tree-optimized -Wnull-dereference" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

struct demangle_component
{

  int type;
  int zzz;

};

struct d_info
{
  struct demangle_component *comps;
  int next_comp;
  int num_comps;
};

void foo (int);
void bar (int);

static struct demangle_component *
d_make_empty (struct d_info *di)
{
  struct demangle_component *p;

  if (di->next_comp >= di->num_comps)
    return ((void *)0);
  p = &di->comps[di->next_comp];
  return p;
}

struct demangle_component *
d_type (struct d_info *di)
{
   struct demangle_component *ret;
   ret = d_make_empty (di);
   foo (ret->type); /* { dg-warning "null pointer dereference" } */
   bar (ret->zzz); /* { dg-warning "null pointer dereference" } */
   return ret;
}

/* We're testing two aspects of isolation here.  First that isolation
   occurs, second that if we have two null dereferences in a block that
   that we delete everything from the first dereferece to the end of the
   block, regardless of which comes first in the immediate use iterator.

   We leave the 0->type in the IL, so expect to see ->type twice.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "->type" 2 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "->type" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "\\.type" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "->zzz" 1 "isolate-paths"} } */
