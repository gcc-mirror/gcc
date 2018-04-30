// { dg-do compile }
// { dg-options "-fno-short-enums" }

typedef union tree_node *tree;
typedef union gimple_statement_d *gimple;
struct vec_prefix { unsigned num_; };
template<typename T> struct vec_t {
    unsigned length (void) const;
    T &operator[] (unsigned);
    vec_prefix prefix_;
    T vec_[1];
};
template<typename T> inline unsigned vec_t<T>::length (void) const {
    return prefix_.num_;
}
template<typename T> T & vec_t<T>::operator[] (unsigned ix) {
    ((void)(__builtin_expect(!(ix < prefix_.num_), 0) ? __builtin_unreachable(), 0 : 0));
    return vec_[ix];
}
enum tree_code { PARM_DECL };
struct tree_base {
    enum tree_code code : 16;
    unsigned default_def_flag : 1;
};
union tree_node {
    struct tree_base base;
};
struct ipa_param_descriptor {
    tree decl;
    unsigned used : 1;
};
typedef struct ipa_param_descriptor ipa_param_descriptor_t;
struct ipa_node_params {
    vec_t<ipa_param_descriptor_t> *descriptors;
};
static inline int ipa_get_param_count (struct ipa_node_params *info) {
    return ((info->descriptors) ? (info->descriptors)->length () : 0);
}
static inline tree ipa_get_param (struct ipa_node_params *info, int i) {
    return ((*(info->descriptors))[i]).decl;
}
static inline void ipa_set_param_used (struct ipa_node_params *info, int i, bool val) {
    ((*(info->descriptors))[i]).used = val;
}
int ipa_get_param_decl_index (struct ipa_node_params *info, tree ptree)
{
  int i, count;
  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    if (ipa_get_param (info, i) == ptree)       return i;
  return -1;
}
bool visit_ref_for_mod_analysis (gimple stmt __attribute__ ((__unused__)),
				 tree op, void *data)
{
  struct ipa_node_params *info = (struct ipa_node_params *) data;
  if (op && ((enum tree_code) (op)->base.code) == PARM_DECL)
    {
      int index = ipa_get_param_decl_index (info, op);
      ((void)(__builtin_expect(!(index >= 0), 0) ? __builtin_unreachable(), 0 : 0));
      ipa_set_param_used (info, index, true);
    }
}	// { dg-warning "control reaches end of non-void function" }
