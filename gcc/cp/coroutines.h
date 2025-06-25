
/* This caches information that we determine about function params,
   their uses and copies in the coroutine frame.  */

struct param_info
{
  tree field_id;     /* The name of the copy in the coroutine frame.  */
  tree copy_var;     /* The local var proxy for the frame copy.  */
  vec<tree *> *body_uses; /* Worklist of uses, void if there are none.  */
  tree frame_type;   /* The type used to represent this parm in the frame.  */
  tree orig_type;    /* The original type of the parm (not as passed).  */
  tree fr_copy_dtor; /* If we need a DTOR on exception, this is it.  */
  bool by_ref;       /* Was passed by reference.  */
  bool pt_ref;       /* Was a pointer to object.  */
  bool rv_ref;       /* Was an rvalue ref.  */
  bool trivial_dtor; /* The frame type has a trivial DTOR.  */
  bool this_ptr;     /* Is 'this' */
  bool lambda_cobj;  /* Lambda capture object */
};

/* Suspend point hash_map.  */

struct suspend_point_info
{
  /* coro frame field type.  */
  tree awaitable_type;
  /* coro frame field name.  */
  tree await_field_id;
};

/* This data set is used when analyzing statements for await expressions.  */

struct susp_frame_data
{
  /* Function-wide.  */
  tree fs_label;		/* The destination for co_returns.  */
  hash_map<tree, suspend_point_info> *suspend_points; /* Not owned.  */
  vec<tree, va_gc> *block_stack; /* Track block scopes.  */
  vec<tree, va_gc> *bind_stack;  /* Track current bind expr.  */
  unsigned await_number = 0;	 /* Which await in the function.  */
  unsigned cond_number = 0;	 /* Which replaced condition in the fn.  */

  /* Temporary values for one statement or expression being analyzed.  */
  /* The set of TRUTH exprs to expand.  */
  hash_set<tree> *truth_aoif_to_expand = nullptr;
  /* Count of awaits in this statement  */
  unsigned saw_awaits = 0;
  /* This expr captures temps by ref.  */
  bool captures_temporary = false;
  /* We must expand a truth_if expression.  */
  bool needs_truth_if_exp = false;
  /* We must handle initializing an awaiter.  */
  bool has_awaiter_init = false;

  susp_frame_data (tree _final_susp, hash_map<tree, suspend_point_info> *_spt)
    : fs_label (_final_susp), suspend_points (_spt)
  {
    block_stack = make_tree_vector ();
    bind_stack = make_tree_vector ();
  }
};

struct local_var_info
{
  tree field_id;
  tree field_idx;
  tree frame_type;
  bool is_lambda_capture;
  bool is_static;
  bool has_value_expr_p;
  location_t def_loc;
};

/* For recording local variable usage.  */

struct local_vars_frame_data
{
  tree *field_list;
  hash_map<tree, local_var_info> *local_var_uses;
  unsigned int nest_depth = 0;
  unsigned int bind_indx = 0;
  location_t loc = UNKNOWN_LOCATION;
  bool saw_capture = false;
  bool local_var_seen = false;

  local_vars_frame_data (tree *_fl, hash_map<tree, local_var_info> *_lvu)
    : field_list (_fl), local_var_uses (_lvu) {}
};

class cp_coroutine_transform {
public:
  cp_coroutine_transform (tree, bool);
  ~cp_coroutine_transform ();

  bool cp_valid_coroutine () const { return valid_coroutine; }
  void apply_transforms ();
  void finish_transforms ();
  tree get_resumer () { return resumer; }
  tree get_destroyer () { return destroyer; }

private:
  tree orig_fn_decl;		 /* The original function decl.  */
  tree orig_fn_body = NULL_TREE; /* The original function body.  */
  location_t fn_start = UNKNOWN_LOCATION;
  location_t fn_end = UNKNOWN_LOCATION;
  tree resumer = error_mark_node;
  tree destroyer = error_mark_node;
  tree coroutine_body = NULL_TREE;
  tree body_blocks = NULL_TREE;

  /* Types for this coroutine.  */
  tree frame_type;
  tree frame_ptr_type;
  tree act_des_fn_type;
  tree act_des_fn_ptr_type;

  /* Cached information about the transformed function.  */
  tree resume_idx_var = NULL_TREE;
  tree fs_label = NULL_TREE;
  hash_map<tree, param_info> param_uses;
  hash_map<tree, suspend_point_info> suspend_points;
  hash_map<tree, local_var_info> local_var_uses;
  vec<tree> param_dtor_list = vNULL;
  tree frame_size = NULL_TREE;
  unsigned int await_count = 0;

  bool inline_p = false;
  bool valid_coroutine = false;

  tree initial_await = error_mark_node;
  tree final_await = error_mark_node;

  void analyze_fn_parms ();
  void wrap_original_function_body ();
  bool build_ramp_function ();
};
