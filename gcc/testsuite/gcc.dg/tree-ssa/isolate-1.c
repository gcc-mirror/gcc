/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-isolate-paths -fdelete-null-pointer-checks -Wnull-dereference" } */
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
   ret->type = 42;		/* { dg-warning "null pointer dereference" } */
   ret->zzz = -1;		/* { dg-warning "null pointer dereference" } */
   return ret;
}

/* We're testing three aspects of isolation here.  First that isolation
   occurs, second that if we have two null dereferences in a block that
   that we delete everything from the first dereferece to the end of the
   block, regardless of which comes first in the immediate use iterator
   and finally that we set the RHS of the store to zero.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "->type = 42" 1 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "->type ={v} 0" 1 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "->zzz" 1 "isolate-paths"} } */
