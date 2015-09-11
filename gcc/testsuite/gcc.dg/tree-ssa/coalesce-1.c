/* { dg-do compile } */

/* { dg-options "-O2 -fdump-rtl-expand-details" } */

typedef long unsigned int size_t;
union tree_node;
typedef union tree_node *tree;
union gimple_statement_d;
typedef union gimple_statement_d *gimple;
typedef const union tree_node *const_tree;
typedef const union gimple_statement_d *const_gimple;
struct gimple_seq_d;
typedef struct gimple_seq_d *gimple_seq;
struct edge_def;
typedef struct edge_def *edge;
struct basic_block_def;
typedef struct basic_block_def *basic_block;
typedef const struct basic_block_def *const_basic_block;
struct tree_exp
{
  tree operands[1];
};
typedef struct ssa_use_operand_d
{
  tree *use;
} ssa_use_operand_t;
struct phi_arg_d
{
  struct ssa_use_operand_d imm_use;
};
union tree_node
{
  struct tree_exp exp;
};
struct function
{
};
extern struct function *cfun;
struct edge_def
{
  unsigned int dest_idx;
};
static __inline__ void
VEC_edge_must_be_pointer_type (void)
{
  (void) ((edge) 1 == (void *) 1);
} typedef struct VEC_edge_base

{
  unsigned num;
  unsigned alloc;
  edge vec[1];
} VEC_edge_base;
typedef struct VEC_edge_none
{
  VEC_edge_base base;
} VEC_edge_none;

static __inline__ edge
VEC_edge_base_index (const VEC_edge_base * vec_, unsigned ix_,
		     const char *file_, unsigned line_, const char *function_)
{
  return vec_->vec[ix_];
}

typedef struct VEC_edge_gc
{
  VEC_edge_base base;
} VEC_edge_gc;
struct basic_block_def
{
  VEC_edge_gc *succs;
};
static __inline__ edge
single_succ_edge (const_basic_block bb)
{
  return (VEC_edge_base_index
	  ((((bb)->succs) ? &((bb)->succs)->base : 0), (0),
	   "/home/gcc/virgin-gcc/gcc/basic-block.h", 563, __FUNCTION__));
}

edge find_edge (basic_block, basic_block);
typedef tree *def_operand_p;
typedef ssa_use_operand_t *use_operand_p;
struct gimple_seq_node_d;
typedef struct gimple_seq_node_d *gimple_seq_node;
struct gimple_seq_node_d
{
  gimple stmt;
};
typedef struct
{
  gimple_seq_node ptr;
  gimple_seq seq;
  basic_block bb;
} gimple_stmt_iterator;
struct gimple_statement_phi
{
  struct phi_arg_d args[1];
};
union gimple_statement_d
{
  struct gimple_statement_phi gimple_phi;
};
extern size_t const gimple_ops_offset_[];
int gimple_statement_structure (gimple);
static __inline__ tree *
gimple_ops (gimple gs)
{
  size_t off;
  off = gimple_ops_offset_[gimple_statement_structure (gs)];
  return (tree *) ((char *) gs + off);
}

static __inline__ tree
gimple_op (const_gimple gs, unsigned i)
{
  return gimple_ops ((((union
			{
			const union gimple_statement_d * _q;
			union gimple_statement_d * _nq;}) (((gs))))._nq))[i];
}

static __inline__ struct phi_arg_d *
gimple_phi_arg (gimple gs, unsigned index)
{
  return &(gs->gimple_phi.args[index]);
}

static __inline__ tree
gimple_switch_label (const_gimple gs, unsigned index)
{
  return gimple_op (gs, index + 1);
}

gimple_stmt_iterator gsi_start_phis (basic_block);
extern basic_block label_to_block_fn (struct function *, tree);

static __inline__ tree
get_use_from_ptr (use_operand_p use)
{
  return *(use->use);
}

static __inline__ use_operand_p
gimple_phi_arg_imm_use_ptr (gimple gs, int i)
{
  return &gimple_phi_arg (gs, i)->imm_use;
}

struct switch_conv_info
{
  basic_block final_bb;
  basic_block switch_bb;
  const char *reason;
  tree *default_values;
};
static struct switch_conv_info info;
void gsi_next (gimple_stmt_iterator *);
int gsi_gsi_start_phis (basic_block);

static void
gather_default_values (tree default_case)
{
  gimple_stmt_iterator gsi;
  basic_block bb =
    (label_to_block_fn ((cfun + 0), default_case->exp.operands[2]));
  edge e;
  int i = 0;
  if (bb == info.final_bb)
    e = find_edge (info.switch_bb, bb);
  else
    e = single_succ_edge (bb);
  for (gsi = gsi_start_phis (info.final_bb);
       gsi_gsi_start_phis (info.final_bb); gsi_next (&gsi))
    {
      gimple phi = gsi.ptr->stmt;
      tree val = get_use_from_ptr (gimple_phi_arg_imm_use_ptr
				   ((((phi))), (((e)->dest_idx))));
      info.default_values[i++] = val;
    }
}

unsigned int gimple_switch_num_labels (gimple);

unsigned char
process_switch (gimple swtch)
{
  unsigned int i, branch_num = gimple_switch_num_labels (swtch);
  tree index_type;
  info.reason = "switch has no labels\n";
  gather_default_values (gimple_switch_label (swtch, 0));
}

/* Verify that out-of-ssa coalescing did its job by verifying there are not
   any partition copies inserted.  */

/* { dg-final { scan-rtl-dump-not "partition copy" "expand"} } */

