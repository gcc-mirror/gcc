/* { dg-do compile } */
/* { dg-skip-if "incompatible options" { ! { arm_thumb1_ok || arm_thumb2_ok } } { "*" } { "" } } */
/* { dg-options "-fno-short-enums -O2 -mthumb -march=armv7-a -mfpu=neon -mfloat-abi=softfp -mtune=cortex-a9 -fno-section-anchors" } */

typedef unsigned int size_t;
__extension__ typedef int __intptr_t;
typedef __intptr_t intptr_t;
typedef union tree_node *tree;
typedef const union tree_node *const_tree;
extern void *ggc_internal_cleared_alloc_stat (size_t )
  __attribute__ ((__malloc__));
enum tree_code {
TREE_LIST=2,
FUNCTION_DECL,
MAX_TREE_CODES=254
};
extern unsigned char tree_contains_struct[MAX_TREE_CODES][64];
struct tree_base {
  enum tree_code code : 16;
};
struct tree_common {
  tree chain;
};
enum tree_node_structure_enum {
TS_COMMON,
TS_DECL_COMMON,
};
extern void tree_contains_struct_check_failed (const_tree,
            const enum tree_node_structure_enum,
            const char *, int, const char *)
  __attribute__ ((__noreturn__));
extern void tree_check_failed (const_tree, const char *, int, const char *,
          ...) __attribute__ ((__noreturn__));
struct tree_list {
  tree value;
};
struct tree_decl_common {
  tree initial;
};
struct tree_function_decl {
  struct function *f;
};
union
                                                         tree_node {
  struct tree_base base;
  struct tree_common common;
  struct tree_decl_common decl_common;
  struct tree_function_decl function_decl;
  struct tree_list list;
};
inline tree
tree_check (tree __t, const char *__f, int __l, const char *__g, enum tree_code __c)
{
  if (((enum tree_code) (__t)->base.code) != __c)
    tree_check_failed (__t, __f, __l, __g, __c, 0);
}
inline tree
contains_struct_check (tree __t, const enum tree_node_structure_enum __s,
                       const char *__f, int __l, const char *__g)
{
  if (tree_contains_struct[((enum tree_code) (__t)->base.code)][__s] != 1)
      tree_contains_struct_check_failed (__t, __s, __f, __l, __g);
}
struct function {
  tree static_chain_decl;
};
enum gimple_code {
    LAST_AND_UNUSED_GIMPLE_CODE
};
struct eh_catch_d
{
  struct eh_catch_d *next_catch;
  struct eh_catch_d *prev_catch;
  tree type_list;
  tree filter_list;
  tree label;
};
struct eh_region_d
{
  struct eh_region_d *outer;
  struct eh_region_d *inner;
  int index;
  union eh_region_u {
    struct eh_region_u_try {
      struct eh_catch_d *first_catch;
    } eh_try;
  } u;
};
typedef struct eh_catch_d *eh_catch;
typedef struct eh_region_d *eh_region;
extern void add_type_for_runtime (tree);
enum LTO_tags
{
  LTO_null = 0,
  LTO_bb0 = 1 + MAX_TREE_CODES + LAST_AND_UNUSED_GIMPLE_CODE,
  LTO_ert_cleanup,
  LTO_NUM_TAGS
};
enum lto_section_type
{
  LTO_section_function_body,
};
struct lto_input_block
{
  const char *data;
  unsigned int p;
  unsigned int len;
};
extern void lto_section_overrun (struct lto_input_block *) __attribute__ ((__noreturn__));
extern void lto_value_range_error (const char *,
       long long, long long,
       long long) __attribute__ ((__noreturn__));
long long streamer_read_hwi (struct lto_input_block *);
static inline unsigned char
streamer_read_uchar (struct lto_input_block *ib)
{
  if (ib->p >= ib->len)
    lto_section_overrun (ib);
  return (ib->data[ib->p++]);
}
static inline long long
streamer_read_hwi_in_range (struct lto_input_block *ib,
     const char *purpose,
     long long min,
     long long max)
{
  long long range = max - min;
  long long val = streamer_read_uchar (ib);
  if (range >= 0xff)
    val |= ((long long)streamer_read_uchar (ib)) << 8;
  if (val < min || val > max)
    lto_value_range_error (purpose, val, min, max);
  return val;
}
static inline enum LTO_tags
streamer_read_record_start (struct lto_input_block *ib)
{
  return (enum LTO_tags)streamer_read_hwi_in_range ((ib), "LTO_tags", 0, (int)(LTO_NUM_TAGS) - 1);
}
struct streamer_hooks {
  tree (*read_tree) (struct lto_input_block *, struct data_in *);
};
extern struct streamer_hooks streamer_hooks;
static struct eh_catch_d *
lto_input_eh_catch_list (struct lto_input_block *ib, struct data_in *data_in,
    eh_catch *last_p)
{
  eh_catch first;
  enum LTO_tags tag;
  *last_p = first = __null;
  tag = streamer_read_record_start (ib);
  while (tag)
    {
      tree list;
      eh_catch n;
      n = ((struct eh_catch_d *)(ggc_internal_cleared_alloc_stat (sizeof (struct eh_catch_d) )));
      n->type_list = streamer_hooks.read_tree(ib, data_in);
      n->filter_list = streamer_hooks.read_tree(ib, data_in);
      n->label = streamer_hooks.read_tree(ib, data_in);
      for (list = n->filter_list; list; list = ((contains_struct_check ((list), (TS_COMMON), "../../../gcc-4.8~svn195526/gcc/lto-streamer-in.c", 275, __FUNCTION__))->common.chain))
 add_type_for_runtime (((tree_check ((list), "../../../gcc-4.8~svn195526/gcc/lto-streamer-in.c", 276, __FUNCTION__, (TREE_LIST)))->list.value));
      if (*last_p)
 (*last_p)->next_catch = n;
      n->prev_catch = *last_p;
      *last_p = n;
      if (first == __null)
 first = n;
      tag = streamer_read_record_start (ib);
    }
  return first;
}
static eh_region
input_eh_region (struct lto_input_block *ib, struct data_in *data_in, int ix)
{
  enum LTO_tags tag;
  eh_region r;
  tag = streamer_read_record_start (ib);
  if (tag == LTO_null)
    return __null;
  r = ((struct eh_region_d *)(ggc_internal_cleared_alloc_stat (sizeof (struct eh_region_d) )));
  r->index = streamer_read_hwi (ib);
  r->outer = (eh_region) (intptr_t) streamer_read_hwi (ib);
  r->inner = (eh_region) (intptr_t) streamer_read_hwi (ib);
  switch (tag)
    {
      case LTO_ert_cleanup:
 {
   struct eh_catch_d *last_catch;
   r->u.eh_try.first_catch = lto_input_eh_catch_list (ib, data_in,
            &last_catch);
 }
 {
   tree l;
     add_type_for_runtime (((tree_check ((l), "../../../gcc-4.8~svn195526/gcc/lto-streamer-in.c", 346, __FUNCTION__, (TREE_LIST)))->list.value));
 }
    }
}
static void
input_eh_regions (struct lto_input_block *ib, struct data_in *data_in,
    struct function *fn)
{
  long long i, root_region, len;
  enum LTO_tags tag;
  tag = streamer_read_record_start (ib);
  if (tag == LTO_null)
    return;
  len = streamer_read_hwi (ib);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
 {
   eh_region r = input_eh_region (ib, data_in, i);
 }
    }
}
static void
input_ssa_names (struct lto_input_block *ib, struct data_in *data_in,
   struct function *fn)
{
  unsigned int i, size;
  while (i)
    {
    }
}
static void
input_struct_function_base (struct function *fn, struct data_in *data_in,
                            struct lto_input_block *ib)
{
  fn->static_chain_decl = streamer_hooks.read_tree(ib, data_in);
}
static void
input_function (tree fn_decl, struct data_in *data_in,
  struct lto_input_block *ib)
{
  struct function *fn;
  enum LTO_tags tag;
  fn = ((tree_check ((fn_decl), "../../../gcc-4.8~svn195526/gcc/lto-streamer-in.c", 807, __FUNCTION__, (FUNCTION_DECL)))->function_decl.f);
  tag = streamer_read_record_start (ib);
  input_struct_function_base (fn, data_in, ib);
  input_ssa_names (ib, data_in, fn);
  input_eh_regions (ib, data_in, fn);
  ((contains_struct_check ((fn_decl), (TS_DECL_COMMON), "../../../gcc-4.8~svn195526/gcc/lto-streamer-in.c", 823, __FUNCTION__))->decl_common.initial) = streamer_hooks.read_tree(ib, data_in);
}
static void
lto_read_body (struct lto_file_decl_data *file_data, tree fn_decl,
        const char *data, enum lto_section_type section_type)
{
  struct data_in *data_in;
  struct lto_input_block ib_main;
  input_function (fn_decl, data_in, &ib_main);
}
void
lto_input_function_body (struct lto_file_decl_data *file_data,
    tree fn_decl, const char *data)
{
  lto_read_body (file_data, fn_decl, data, LTO_section_function_body);
}

