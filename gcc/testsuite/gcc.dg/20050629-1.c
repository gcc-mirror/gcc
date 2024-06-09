/* { dg-do compile } */
/* { dg-options "-O2 -w -fpermissive" } */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

/* This file was automatically reduced from tree-ssa-operands.c.  It
   contains many warnings, but it exposes a copy propagation bug that
   is somewhat difficult to reproduce otherwise.  */

__extension__ typedef __SIZE_TYPE__ size_t;
  extern void fancy_abort (const char *, int, const char *) __attribute__ ((__noreturn__));
  typedef union tree_node *tree;
  enum tree_code {
 TREE_LIST, ARRAY_TYPE, RECORD_TYPE, UNION_TYPE, QUAL_UNION_TYPE, STRING_CST, LT_EXPR, GE_EXPR, LABEL_EXPR, ASM_EXPR, SSA_NAME, };
  enum tree_code_class {
   tcc_type,   tcc_reference,   tcc_expression };
  extern const enum tree_code_class tree_code_type[];
  extern const unsigned char tree_code_length[];
  struct tree_common {
   union tree_ann_d *ann;
   __extension__ enum tree_code code : 8;
 };
  struct tree_string {
   char str[1];
 };
  struct tree_list {
   tree purpose;
   tree value;
 };
  struct tree_exp {
   tree     operands[1];
 };
  typedef struct ssa_use_operand_d {
   struct ssa_use_operand_d* prev;
   struct ssa_use_operand_d* next;
   tree stmt;
   tree * use;
 }
  ssa_use_operand_t;
  struct tree_ssa_name {
   struct ssa_use_operand_d imm_uses;
 };
  union tree_node {
   struct tree_common common;
   struct tree_string string;
   struct tree_list list;
   struct tree_exp exp;
   struct tree_ssa_name ssa_name;
 };
  typedef struct bitmap_head_def *bitmap;
  typedef union varray_data_tag {
   char c[1];
   int i[1];
   tree * tp[1];
 }
  varray_data;
  struct varray_head_tag {
   size_t num_elements;
   size_t elements_used;
   varray_data data;
 };
  typedef struct varray_head_tag *varray_type;
  extern void varray_check_failed (varray_type, size_t, const char *, int,      const char *) __attribute__ ((__noreturn__));
  typedef ssa_use_operand_t *use_operand_p;
  struct use_optype_d {
   struct use_optype_d *next;
   struct ssa_use_operand_d use_ptr;
 };
  typedef struct use_optype_d *use_optype_p;
  struct ssa_operand_memory_d {
   char mem[(2048 - sizeof (void *))];
 };
  struct stmt_operands_d {
   struct use_optype_d * use_ops;
 };
  struct ptr_info_def {
   tree name_mem_tag;
 };
  typedef struct subvar *subvar_t;
  struct var_ann_d {
   size_t uid;
 };
  struct stmt_ann_d {
   unsigned modified : 1;
   struct stmt_operands_d operands;
   bitmap addresses_taken;
 };
  typedef struct var_ann_d *var_ann_t;
  typedef struct stmt_ann_d *stmt_ann_t;
  static __inline__ var_ann_t var_ann (tree t) {
 }
  static __inline__ stmt_ann_t stmt_ann (tree t) {
   ((void)(!(is_gimple_stmt (t)) ? fancy_abort ("/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-flow-inline.h", 55, __FUNCTION__), 0 : 0));
   return (stmt_ann_t) t->common.ann;
 }
  static __inline__ unsigned char stmt_modified_p (tree t) {
   stmt_ann_t ann = stmt_ann (t);
   return ann ? ann->modified : 1;
 }
  static __inline__ void delink_imm_use (ssa_use_operand_t *linknode) {
   if (linknode->prev == ((void *)0))     return;
   linknode->prev->next = linknode->next;
   linknode->next->prev = linknode->prev;
   linknode->prev = ((void *)0);
   linknode->next = ((void *)0);
 }
  static __inline__ void link_imm_use_to_list (ssa_use_operand_t *linknode, ssa_use_operand_t *list) {
   linknode->prev = list;
   linknode->next = list->next;
   list->next = linknode;
 }
  static __inline__ void link_imm_use (ssa_use_operand_t *linknode, tree def) {
   ssa_use_operand_t *root;
   if (!def || ((enum tree_code) (def)->common.code) != SSA_NAME)     linknode->prev = ((void *)0);
   else     {
       root = &(__extension__ ({ const tree __t = (def); if (((enum tree_code) (__t)->common.code) != (SSA_NAME)) tree_check_failed (__t, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-flow-inline.h", 222, __FUNCTION__, (SSA_NAME), 0); __t; }
)->ssa_name.imm_uses);
       link_imm_use_to_list (linknode, root);
     }
 }
  static __inline__ void link_imm_use_stmt (ssa_use_operand_t *linknode, tree def, tree stmt) {
     link_imm_use (linknode, def);
 }
  struct ggc_root_tab {
 };
  struct opbuild_list_d {
   varray_type vars;
   varray_type uid;
   varray_type next;
   int first;
   unsigned num;
 };
  static struct opbuild_list_d build_uses;
  static struct opbuild_list_d build_v_may_defs;
  static struct ssa_operand_memory_d *operand_memory = ((void *)0);
  static unsigned operand_memory_index;
  static use_optype_p free_uses = ((void *)0);
  static __inline__ int opbuild_next (struct opbuild_list_d *list, int prev) {
   return __extension__ (*({
 varray_type const _va = (list->next);
 const size_t _n = (prev);
 if (_n >= _va->num_elements) varray_check_failed (_va, _n, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 323, __FUNCTION__);
 &_va->data.i[_n];
 }
));
 }
  static __inline__ tree * opbuild_elem_real (struct opbuild_list_d *list, int elem) {
   return __extension__ (*({
 varray_type const _va = (list->vars);
 const size_t _n = (elem);
 if (_n >= _va->num_elements) varray_check_failed (_va, _n, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 332, __FUNCTION__);
 &_va->data.tp[_n];
 }
));
 }
  static __inline__ void opbuild_clear (struct opbuild_list_d *list) {
   ((list->vars)->elements_used = 0);
   ((list->next)->elements_used = 0);
 }
  static __inline__ void * ssa_operand_alloc (unsigned size) {
   char *ptr;
   if (operand_memory_index + size >= (2048 - sizeof (void *)))     {
       ptr = ggc_alloc_stat (sizeof (struct ssa_operand_memory_d) );
     }
   ptr = &(operand_memory->mem[operand_memory_index]);
   return ptr;
 }
  static __inline__ void correct_use_link (use_operand_p ptr, tree stmt) {
   use_operand_p prev;
   tree root;
   prev = ptr->prev;
   if (prev)     {
       unsigned char stmt_mod = 1;
       while (stmt_mod)  {    while (prev->stmt == stmt || prev->stmt == ((void *)0))      prev = prev->prev;    if (prev->use == ((void *)0))      stmt_mod = 0;    else      if ((stmt_mod = stmt_modified_p (prev->stmt)))        prev = prev->prev;  }
       if (prev->use == ((void *)0))  root = prev->stmt;
       else  root = *(prev->use);
       if (root == *(ptr->use))  return;
     }
   delink_imm_use (ptr);
   link_imm_use (ptr, *(ptr->use));
 }
  static __inline__ struct use_optype_d * alloc_use (void) {
   struct use_optype_d *ret;
     {
       free_uses = free_uses->next;
     }
     ret = (struct use_optype_d *)ssa_operand_alloc (sizeof (struct use_optype_d));
   return ret;
 }
  static __inline__ void finalize_ssa_use_ops (tree stmt) {
   int new_i;
   struct use_optype_d *old_ops, *ptr, *last;
   tree * old_base;
   struct use_optype_d new_list;
   last = &new_list;
   if (old_ops)     old_base = ((old_ops)->use_ptr.use);
   while (old_ops && new_i != -1)     {
       tree * new_base = opbuild_elem_real (&build_uses, (new_i));
       if (old_base == new_base)         {    last->next = old_ops;    last = old_ops;    correct_use_link ((&((last)->use_ptr)), stmt);    old_ops = old_ops->next;    new_i = opbuild_next (&build_uses, new_i);  }
       else         if (old_base < new_base)    {      use_operand_p use_p = (&((old_ops)->use_ptr));      delink_imm_use (use_p);      old_ops = old_ops->next;      ptr->next = free_uses;    }
  else    {      ptr = alloc_use ();      (ptr)->use_ptr.use = (opbuild_elem_real (&build_uses, (new_i))); link_imm_use_stmt (&((ptr)->use_ptr), *(opbuild_elem_real (&build_uses, (new_i))), (stmt));      new_i = opbuild_next (&build_uses, new_i);    }
       if (old_ops)         old_base = ((old_ops)->use_ptr.use);
     }
   for ( ;
  new_i != -1;
  new_i = opbuild_next (&build_uses, new_i))     {
       ptr = alloc_use ();
       (ptr)->use_ptr.use = (opbuild_elem_real (&build_uses, (new_i)));
 link_imm_use_stmt (&((ptr)->use_ptr), *(opbuild_elem_real (&build_uses, (new_i))), (stmt));
     }
     {
  {    use_operand_p use_p = (&((ptr)->use_ptr));    delink_imm_use (use_p);  }
     }
   (stmt_ann (stmt)->operands.use_ops) = new_list.next;
   {
     unsigned x = 0;
     for (ptr = (stmt_ann (stmt)->operands.use_ops);
 ptr;
 ptr = ptr->next)       x++;
   }
 }
  finalize_ssa_uses (tree stmt) {
   {
     unsigned x;
       ((void)(!(*(opbuild_elem_real (&build_uses, x)) != stmt) ? fancy_abort ("/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 581, __FUNCTION__), 0 : 0));
   }
   finalize_ssa_use_ops (stmt);
   opbuild_clear (&build_uses);
 }
  finalize_ssa_v_may_def_ops (tree stmt) {
   int new_i;
     {
     }
   for ( ;
  new_i != -1;
  new_i = opbuild_next (&build_v_may_defs, new_i))     {
     }
   {
   }
 }
  get_expr_operands (tree stmt, tree *expr_p, int flags) {
   enum tree_code code;
   tree expr = *expr_p;
     {
       {  subvar_t svars;  if (var_can_have_subvars (expr)      && (svars = get_subvars_for_var (expr)))    {    }    {    }       }
       {    {      if (code == LT_EXPR   || code == GE_EXPR)        {   swap_tree_operands (stmt,         &__extension__ (*({const tree __t = __extension__ ({ const tree __t = (expr); char const __c = tree_code_type[(int) (((enum tree_code) (__t)->common.code))]; if (!((__c) >= tcc_reference && (__c) <= tcc_expression)) tree_class_check_failed (__t, tcc_expression, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1435, __FUNCTION__); __t; }); const int __i = (0); if (__i < 0 || __i >= tree_code_length[(int) (((enum tree_code) (__t)->common.code))]) tree_operand_check_failed (__i, ((enum tree_code) (__t)->common.code), "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1435, __FUNCTION__); &__t->exp.operands[__i]; })),         &__extension__ (*({const tree __t = __extension__ ({ const tree __t = (expr); char const __c = tree_code_type[(int) (((enum tree_code) (__t)->common.code))]; if (!((__c) >= tcc_reference && (__c) <= tcc_expression)) tree_class_check_failed (__t, tcc_expression, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1436, __FUNCTION__); __t; }); const int __i = (1); if (__i < 0 || __i >= tree_code_length[(int) (((enum tree_code) (__t)->common.code))]) tree_operand_check_failed (__i, ((enum tree_code) (__t)->common.code), "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1436, __FUNCTION__); &__t->exp.operands[__i]; })));        }        {        }    }       }
     }
 }
  get_asm_expr_operands (tree stmt) {
   int noutputs = list_length (__extension__ (*({
const tree __t = __extension__ ({ const tree __t = (__extension__ ({ const tree __t = (stmt); if (((enum tree_code) (__t)->common.code) != (ASM_EXPR)) tree_check_failed (__t, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1495, __FUNCTION__, (ASM_EXPR), 0); __t; })); char const __c = tree_code_type[(int) (((enum tree_code) (__t)->common.code))]; if (!((__c) >= tcc_reference && (__c) <= tcc_expression)) tree_class_check_failed (__t, tcc_expression, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1495, __FUNCTION__); __t; }
);
 const int __i = (1);
 if (__i < 0 || __i >= tree_code_length[(int) (((enum tree_code) (__t)->common.code))]) tree_operand_check_failed (__i, ((enum tree_code) (__t)->common.code), "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1495, __FUNCTION__);
 &__t->exp.operands[__i];
 }
)));
   const char **oconstraints     = (const char **) __builtin_alloca((noutputs) * sizeof (const char *));
   int i;
   tree link;
   const char *constraint;
   unsigned char allows_mem, allows_reg, is_inout;
     {
       oconstraints[i] = constraint  = ((const char *)(__extension__ ({ const tree __t = ((__extension__ ({ const tree __t = ((__extension__ ({ const tree __t = (link); if (((enum tree_code) (__t)->common.code) != (TREE_LIST)) tree_check_failed (__t, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1506, __FUNCTION__, (TREE_LIST), 0); __t; })->list.purpose)); if (((enum tree_code) (__t)->common.code) != (TREE_LIST)) tree_check_failed (__t, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1506, __FUNCTION__, (TREE_LIST), 0); __t; })->list.value)); if (((enum tree_code) (__t)->common.code) != (STRING_CST)) tree_check_failed (__t, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1506, __FUNCTION__, (STRING_CST), 0); __t; }
)->string.str));
       parse_output_constraint (&constraint, i, 0, 0,    &allows_mem, &allows_reg, &is_inout);
       }
 }
  get_indirect_ref_operands (tree stmt, tree expr, int flags) {
   tree *pptr = &__extension__ (*({
const tree __t = __extension__ ({ const tree __t = (expr); char const __c = tree_code_type[(int) (((enum tree_code) (__t)->common.code))]; if (!((__c) >= tcc_reference && (__c) <= tcc_expression)) tree_class_check_failed (__t, tcc_expression, "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1591, __FUNCTION__); __t; }
);
 const int __i = (0);
 if (__i < 0 || __i >= tree_code_length[(int) (((enum tree_code) (__t)->common.code))]) tree_operand_check_failed (__i, ((enum tree_code) (__t)->common.code), "/home/cygnus/dnovillo/gcc/src.ppc64/gcc/tree-ssa-operands.c", 1591, __FUNCTION__);
 &__t->exp.operands[__i];
 }
));
   tree ptr = *pptr;
     {
       struct ptr_info_def *pi = ((void *)0);
       if (((enum tree_code) (ptr)->common.code) == SSA_NAME    && pi->name_mem_tag)  {  }
     }
 }
  note_addressable (tree var, stmt_ann_t s_ann) {
   subvar_t svars;
     {
       if (var_can_have_subvars (var)    && (svars = get_subvars_for_var (var)))  bitmap_set_bit (s_ann->addresses_taken, var_ann (var)->uid);
     }
 }
  const struct ggc_root_tab gt_ggc_r_gt_tree_ssa_operands_h[] = {
   {
     &operand_memory,   }
,   {
   }
,   {
   }
,   {
   }
, };
