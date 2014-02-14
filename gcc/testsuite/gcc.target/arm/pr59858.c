/* { dg-do compile } */
/* { dg-options "-march=armv5te -fno-builtin -mfloat-abi=soft -mthumb -fno-stack-protector -Os -fno-tree-loop-optimize -fno-tree-dominator-opts -fPIC -w" } */
/* { dg-skip-if "Incompatible command line options: -mfloat-abi=soft -mfloat-abi=hard" { *-*-* } { "-mfloat-abi=hard" } { "" } } */

typedef enum {
 REG_ENOSYS = -1,
} reg_errcode_t;
typedef unsigned long int bitset_word_t;
typedef bitset_word_t bitset_t[(256 / (sizeof (bitset_word_t) * 8))];
typedef bitset_word_t *re_bitset_ptr_t;
typedef const bitset_word_t *re_const_bitset_ptr_t;
typedef struct {
 int nelem;
 int *elems;
} re_node_set;
typedef enum {
 CHARACTER = 1,
} re_token_type_t;
typedef struct {
 re_token_type_t type:8;
 unsigned int word_char:1;
} re_token_t;
struct re_string_t {
 const unsigned char *raw_mbs;
 int raw_mbs_idx;
 int cur_idx;
 unsigned int tip_context;
 re_const_bitset_ptr_t word_char;
};
typedef struct re_string_t re_string_t;
typedef struct re_dfa_t re_dfa_t;
struct re_dfastate_t {
 re_node_set nodes;
};
typedef struct re_dfastate_t re_dfastate_t;
typedef struct {
 re_dfastate_t **array;
} state_array_t;
typedef struct {
 state_array_t path;
} re_sub_match_last_t;
typedef struct {
 int nlasts;
 re_sub_match_last_t **lasts;
} re_sub_match_top_t;
typedef struct {
 re_string_t input;
 const re_dfa_t *dfa;
 int nsub_tops;
 re_sub_match_top_t **sub_tops;
} re_match_context_t;
struct re_dfa_t {
 re_token_t *nodes;
 re_bitset_ptr_t sb_char;
 int mb_cur_max;
 bitset_t word_char;
} bracket_elem_t;
static reg_errcode_t
re_string_reconstruct (
 re_string_t * pstr,
 int idx,
 int eflags
)
{
 int offset = idx - pstr->raw_mbs_idx;
 int c = pstr->raw_mbs[pstr->raw_mbs_idx + offset - 1];
 pstr->tip_context = ((pstr->word_char[c] & ((bitset_word_t) 1)) ? : (c));
}

static void match_ctx_clean (
 re_match_context_t *
);
static int check_matching (
);
static re_dfastate_t *transit_state (
);
static int build_trtable (
);
re_search_internal (int eflags
)
{
 reg_errcode_t err;
 int incr;
 int
  match_first,
  match_last = -1;
 re_match_context_t mctx;
 err = re_string_allocate (&mctx.input);
 for (;; match_first += incr)
  {
   err = re_string_reconstruct (&mctx.input, match_first, eflags);
   err = re_string_reconstruct (&mctx.input, match_first, eflags);
   match_last = check_matching (&mctx, &match_first);
   match_ctx_clean (&mctx);
  }
}

check_matching (re_match_context_t * mctx, int *p_match_first
)
{
 int cur_str_idx = ((&mctx->input)->cur_idx);
 re_dfastate_t *cur_state;
 int next_start_idx = cur_str_idx;
 cur_state = transit_state (mctx, cur_state);
 *p_match_first += next_start_idx;
}

static re_dfastate_t *
transit_state (
 re_match_context_t * mctx,
 re_dfastate_t * state
)
{
 if (!build_trtable (mctx->dfa, state))
  {
  }
}

build_trtable (const re_dfa_t * dfa,
	             re_dfastate_t * state
)
{
 int i,
  j;
 bitset_t accepts;
 const re_node_set *cur_nodes = &state->nodes;
 for (i = 0; i < cur_nodes->nelem; ++i)
  {
   re_token_t *node = &dfa->nodes[cur_nodes->elems[i]];
   re_token_type_t type = node->type;
   {
    if (dfa->mb_cur_max > 1)
     bitset_merge (accepts, dfa->sb_char);
    {
     bitset_word_t any_set = 0;
     if (type == CHARACTER && !node->word_char)
      any_set |= (accepts[j] &= (dfa->word_char[j] | ~dfa->sb_char[j]));
     else
      for (j = 0; j < (256 / (sizeof (bitset_word_t) * 8)); ++j)
       any_set |= (accepts[j] &= dfa->word_char[j]);
    }
   }
  }
}

static void
match_ctx_clean (
 re_match_context_t * mctx
)
{
 int st_idx;
 for (st_idx = 0; st_idx < mctx->nsub_tops; ++st_idx)
  {
   int sl_idx;
   re_sub_match_top_t *top = mctx->sub_tops[st_idx];
   for (sl_idx = 0; sl_idx < top->nlasts; ++sl_idx)
    {
     re_sub_match_last_t *last = top->lasts[sl_idx];
     free (last->path.array);
    }
  }
}

