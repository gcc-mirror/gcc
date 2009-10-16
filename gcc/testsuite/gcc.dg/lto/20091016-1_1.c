typedef struct VEC_constructor_elt_gcx { } VEC_constructor_elt_gc;
#include "20091016-1_a.h"
struct gcc_target {
    void (* builtin_vec_perm) (tree*);
};
extern struct gcc_target targetm;
void dwarf2out_begin_prologue (tree t)
{
    (*targetm.builtin_vec_perm) (&t);
}
struct die_arg_entry_struct {
    tree arg;
};
void *gt_pch_p_20VEC_die_arg_entry_gc (struct die_arg_entry_struct *vec)
{
    return &(vec->arg);
}

