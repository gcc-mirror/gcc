// { dg-do compile }
// { dg-options "-O2 -Wuninitialized" }

typedef int size_t;
typedef struct {
} max_align_t;
typedef struct tree_node *tree;
struct ht_identifier {
  char str;
  int len;
};
struct cpp_hashnode {
  ht_identifier ident;
};
tree get_identifier_with_length(char *, size_t);
struct cpp_reader *parse_in;
typedef int edit_distance_t;
edit_distance_t get_edit_distance(char *);
template < typename > struct edit_distance_traits;
edit_distance_t get_edit_distance_cutoff(size_t);
template < typename GOAL_TYPE, typename CANDIDATE_TYPE > class best_match {
public:
  typedef CANDIDATE_TYPE candidate_t;
  typedef edit_distance_traits< candidate_t > candidate_traits;
  best_match(GOAL_TYPE)
      : m_goal(), m_goal_len(), m_best_candidate(), m_best_distance() {}
  void consider(candidate_t candidate) {
    size_t candidate_len = candidate_traits::get_length(candidate);
    char candidate_str;
    edit_distance_t dist = get_edit_distance(&candidate_str);
    bool is_better = false;
    if (dist)
      is_better = true;
    if (is_better) {
      m_best_candidate = candidate;
      m_best_candidate_len = candidate_len;
    }
  }
  void set_best_so_far(CANDIDATE_TYPE) {}
  candidate_t get_best_meaningful_candidate() {
    edit_distance_t __trans_tmp_1;
    if (m_best_candidate) {
      size_t candidate_len = m_best_candidate_len;
      __trans_tmp_1 = get_edit_distance_cutoff(candidate_len); // { dg-warning "may be used uninitialized" }
    }
    edit_distance_t cutoff = __trans_tmp_1;
    if (cutoff)
      ;
    return m_best_candidate;
  }
  char m_goal;
  size_t m_goal_len;
  candidate_t m_best_candidate;
  edit_distance_t m_best_distance;
  size_t m_best_candidate_len;
};
template <> struct edit_distance_traits< tree > {
  static size_t get_length(tree);
};
class name_hint {};
class best_macro_match : public best_match< tree, cpp_hashnode * > {
public:
  best_macro_match(cpp_reader *);
};
struct c_binding {
  tree id;
  c_binding *prev;
};
struct c_scope {
  c_scope *outer;
  c_binding bindings;
} * current_scope;
tree lookup_name_fuzzy_name;
void lookup_name_fuzzy() {
  bool consider_implementation_names = 0;
  best_match< tree, tree > bm(lookup_name_fuzzy_name);
  for (c_scope *scope = current_scope; current_scope;
       scope = scope->outer)
    for (c_binding *binding = &scope->bindings; binding;
         binding = binding->prev)
      if (!consider_implementation_names)
        bm.consider(binding->id);
  best_macro_match bmm(parse_in);
  cpp_hashnode *best_macro = bmm.get_best_meaningful_candidate();
  if (best_macro) {
    char id = best_macro->ident.str;
    tree macro_as_identifier =
        get_identifier_with_length(&id, best_macro->ident.len);
    bm.set_best_so_far(macro_as_identifier);
  }
  tree best = bm.get_best_meaningful_candidate();
  if (best)
    name_hint();
}
