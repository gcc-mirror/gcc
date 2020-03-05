/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=long" } */
/* { dg-skip-if "different ABI" { x86_64-*-mingw* } } */

typedef union tree_node *tree;
typedef const union tree_node *const_tree;
typedef struct {
  unsigned long long low;
  long long high;
} double_int;
struct real_value {
};
struct real_format {
  int has_signed_zero;
};
extern const struct real_format *   real_format_for_mode[];
extern int real_isnegzero (const struct real_value *);
enum tree_code { REAL_CST, SSA_NAME };
struct tree_base {
  enum tree_code code : 16;
  union {
    unsigned int version;
  }
  u;
};
extern void tree_check_failed (const_tree, const char *, int, const char *,           ...) __attribute__ ((__noreturn__));
union tree_node {
  struct tree_base base;
};
inline tree tree_check (tree __t, const char *__f, int __l, const char *__g, enum tree_code __c) {
  if (((enum tree_code) (__t)->base.code) != __c)
    tree_check_failed (__t, __f, __l, __g, __c, 0);
  return __t;
}
struct prop_value_d {
  int lattice_val;
  tree value;
  double_int mask;
};
typedef struct prop_value_d prop_value_t;
static prop_value_t *const_val;
static void canonicalize_float_value (prop_value_t *);
typedef void (*ssa_prop_visit_stmt_fn) (prop_value_t);
typedef void (*ssa_prop_visit_phi_fn) (void);
typedef void (*ssa_prop_fold_stmt_fn) (void *gsi);
typedef void (*ssa_prop_get_value_fn) ( prop_value_t *val);
void ssa_propagate (ssa_prop_visit_stmt_fn, ssa_prop_visit_phi_fn);
int substitute_and_fold (ssa_prop_get_value_fn, ssa_prop_fold_stmt_fn);
void ccp_fold_stmt (void *);
static void get_constant_value (prop_value_t *val) {
  canonicalize_float_value (val);
}
static void canonicalize_float_value (prop_value_t *val) {
  int mode;
  struct real_value d;
  if (val->lattice_val != 1
      || ((enum tree_code) (val->value)->base.code) != REAL_CST)
    return;
  mode = val->lattice_val;
  if (real_format_for_mode[mode]->has_signed_zero && real_isnegzero (&d))
    ccp_fold_stmt (0);
}
static void set_lattice_value (tree var, prop_value_t new_val) {
  prop_value_t *old_val = &const_val[(tree_check ((var), "",
						  0, "",
						  (SSA_NAME)))->base.u.version];
  canonicalize_float_value (&new_val);
  canonicalize_float_value (old_val);
}
static void ccp_visit_phi_node (void) {
  prop_value_t new_val;
  set_lattice_value (0, new_val);
}
static void ccp_visit_stmt (prop_value_t v) {
  set_lattice_value (0, v);
}
unsigned int do_ssa_ccp (void) {
  ssa_propagate (ccp_visit_stmt, ccp_visit_phi_node);
  substitute_and_fold (get_constant_value, ccp_fold_stmt);
  return 0;
}
