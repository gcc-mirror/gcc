/* { dg-do compile } */

/* We used to fail because GCC didn't expect always inline to be inlined at
   -O0.  */
typedef union tree_node *tree;
typedef struct c_pretty_print_info c_pretty_printer;


void pp_c_string_literal (c_pretty_printer *, tree);


static __inline__  __attribute__((always_inline)) void
pp_c_shift_expression (c_pretty_printer *pp, tree e)
{
}

static void
pp_c_relational_expression (c_pretty_printer *pp, tree e)
{
        pp_c_shift_expression (pp, e);
}
