/* { dg-additional-options "-std=c++20 -mlra -fpic -w " }  */
/* { dg-do compile }  */

   typedef unsigned int size_t;
        extern "C" {
      typedef signed int __int32_t;
      typedef struct {
    }
      __fpos64_t;
      }
        extern "C++" {
      namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp, _Tp __v> struct integral_constant {
  using value_type = _Tp;
  constexpr operator value_type() const noexcept {
  }
  };
    }
      typedef __int32_t int32_t;
      typedef struct {
    }
      ldiv_t;
      }
        extern "C" {
      typedef unsigned int hashval_t;
      enum insert_option {
   NO_INSERT, INSERT};
      }
        typedef const union tree_node *const_tree;
          struct varpool_node;
        struct cgraph_edge;
        template<typename T> inline T * ggc_alloc () {
      }
        struct vl_embed {
      };
        struct vl_ptr {
      };
        struct va_heap {
      typedef vl_ptr default_layout;
      };
       struct va_gc {
      typedef vl_embed default_layout;
      };
        template<typename T, typename A = va_heap, typename L = typename A::default_layout> struct vec {
      };
        template<typename T, typename A, typename L> T* begin (vec<T,A,L> *v) {
      }
        template<typename T, typename A, typename L> T* end (vec<T,A,L> *v) {
      }
        struct vnull {
      };
        template<typename T, typename A> struct vec<T, A, vl_embed> {
      bool is_empty (void) const {
    }
      T &operator[] (unsigned);
      };
        template<typename T, typename A> inline unsigned vec_safe_length (const vec<T, A, vl_embed> *v) {
      }
        template<typename T, size_t N = 0> class auto_vec;
        template<typename T> struct vec<T, va_heap, vl_ptr> {
      public: vec () = default;
      vec (vnull): m_vec () {
    }
      T &operator[] (unsigned ix) {
    return (*m_vec)[ix];
    }
      vec<T, va_heap, vl_embed> *m_vec;
      };
        template<typename T, size_t N > class auto_vec : public vec<T, va_heap> {
      public: auto_vec () {
    }
      auto_vec (size_t s ) {
    }
      };
        enum mem_alloc_origin {
      HASH_TABLE_ORIGIN, HASH_MAP_ORIGIN, HASH_SET_ORIGIN, VEC_ORIGIN, BITMAP_ORIGIN, GGC_ORIGIN, ALLOC_POOL_ORIGIN, MEM_ALLOC_ORIGIN_LENGTH };
        template <typename Type> struct typed_noop_remove {
      };
        template <typename Type> struct int_hash_base : typed_noop_remove <Type> {
      typedef Type value_type;
      };
        template <typename Type, Type Empty, Type Deleted = Empty> struct int_hash : int_hash_base <Type> {
     };
        template <typename T> struct default_hash_traits : T {
     };
        template <typename H, typename Value> struct simple_hashmap_traits {
      typedef typename H::value_type key_type;
      static inline hashval_t hash (const key_type &);
      };
        template <typename Type> struct xcallocator {
      };
        template <typename Descriptor, bool Lazy = false, template<typename Type> class Allocator = xcallocator> class hash_table {
      typedef typename Descriptor::value_type value_type;
      typedef typename Descriptor::compare_type compare_type;
      public: explicit hash_table (size_t, bool ggc = false, bool sanitize_eq_and_hash = true, bool gather_mem_stats = 0, mem_alloc_origin origin = HASH_TABLE_ORIGIN );
      value_type *find_slot_with_hash (const compare_type &comparable, hashval_t hash, enum insert_option insert);
      };
        template<typename Key, typename Value, typename Traits = simple_hashmap_traits<default_hash_traits<Key>, Value> > class hash_map;
        class mem_location {
      };
        const size_t default_hash_map_size = 13;
        template<typename KeyId, typename Value, typename Traits > class hash_map {
      typedef typename Traits::key_type Key;
      struct hash_entry {
    Value m_value;
    typedef hash_entry value_type;
    typedef Key compare_type;
    };
      public: explicit hash_map (size_t n = default_hash_map_size, bool ggc = false, bool sanitize_eq_and_hash = true, bool gather_mem_stats = 0 ) : m_table (n, ggc, sanitize_eq_and_hash, gather_mem_stats, HASH_MAP_ORIGIN ) {
    }
      Value &get_or_insert (const Key &k, bool *existed = nullptr) {
    hash_entry *e = m_table.find_slot_with_hash (k, Traits::hash (k), INSERT);
    return e->m_value;
    }
      hash_table<hash_entry> m_table;
      };
        enum optgroup_flag {
      OPTGROUP_NONE = 0, OPTGROUP_IPA = (1 << 1), OPTGROUP_LOOP = (1 << 2), OPTGROUP_INLINE = (1 << 3), OPTGROUP_OMP = (1 << 4), OPTGROUP_VEC = (1 << 5), OPTGROUP_OTHER = (1 << 6), OPTGROUP_ALL = (OPTGROUP_IPA | OPTGROUP_LOOP | OPTGROUP_INLINE | OPTGROUP_OMP | OPTGROUP_VEC | OPTGROUP_OTHER) };
        typedef enum optgroup_flag optgroup_flags_t;
        enum reorder_blocks_algorithm {
      };
        enum tree_node_structure_enum {
      TS_BASE, TS_TYPED, TS_COMMON, TS_INT_CST, TS_POLY_INT_CST, TS_REAL_CST, TS_FIXED_CST, TS_VECTOR, TS_STRING, TS_COMPLEX, TS_IDENTIFIER, TS_DECL_MINIMAL, TS_DECL_COMMON, TS_DECL_WRTL, TS_DECL_NON_COMMON, TS_DECL_WITH_VIS, TS_FIELD_DECL, TS_VAR_DECL, TS_PARM_DECL, TS_LABEL_DECL, TS_RESULT_DECL, TS_CONST_DECL, TS_TYPE_DECL, TS_FUNCTION_DECL, TS_TRANSLATION_UNIT_DECL, TS_TYPE_COMMON, TS_TYPE_WITH_LANG_SPECIFIC, TS_TYPE_NON_COMMON, TS_LIST, TS_VEC, TS_EXP, TS_SSA_NAME, TS_BLOCK, TS_BINFO, TS_STATEMENT_LIST, TS_CONSTRUCTOR, TS_OMP_CLAUSE, TS_OPTIMIZATION, TS_TARGET_OPTION, LAST_TS_ENUM };
        struct tree_base {
      union {
    struct {
  }
    bits;
    }
      u;
      };
        struct tree_typed {
      };
        inline const_tree contains_struct_check (const_tree __t, const enum tree_node_structure_enum __s, const char *__f, int __l, const char *__g) {
      }
        struct gimple {
      };
        typedef enum {
      TV_NONE, TV_TOTAL, TV_PHASE_SETUP, TV_PHASE_PARSING, TV_PHASE_DEFERRED, TV_PHASE_LATE_PARSING_CLEANUPS, TV_PHASE_OPT_GEN, TV_PHASE_LATE_ASM, TV_PHASE_STREAM_IN, TV_PHASE_STREAM_OUT, TV_PHASE_FINALIZE, TV_NAME_LOOKUP, TV_OVERLOAD, TV_GC, TV_DUMP, TV_PCH_SAVE, TV_PCH_CPP_SAVE, TV_PCH_PTR_REALLOC, TV_PCH_PTR_SORT, TV_PCH_RESTORE, TV_PCH_CPP_RESTORE, TV_CGRAPH, TV_CGRAPHOPT, TV_CGRAPH_FUNC_EXPANSION, TV_CGRAPH_IPA_PASSES, TV_IPA_ODR, TV_IPA_FNSUMMARY, TV_IPA_UNREACHABLE, TV_IPA_INHERITANCE, TV_IPA_VIRTUAL_CALL, TV_IPA_DEVIRT, TV_IPA_CONSTANT_PROP, TV_IPA_INLINING, TV_IPA_FNSPLIT, TV_IPA_COMDATS, TV_IPA_OPT, TV_IPA_LTO_DECOMPRESS, TV_IPA_LTO_COMPRESS, TV_IPA_LTO_OUTPUT, TV_IPA_LTO_GIMPLE_IN, TV_IPA_LTO_GIMPLE_OUT, TV_IPA_LTO_DECL_IN, TV_IPA_LTO_DECL_OUT, TV_IPA_LTO_CTORS_IN, TV_IPA_LTO_CTORS_OUT, TV_IPA_LTO_CGRAPH_IO, TV_IPA_LTO_DECL_MERGE, TV_IPA_LTO_CGRAPH_MERGE, TV_LTO, TV_WHOPR_WPA, TV_WHOPR_WPA_IO, TV_WHOPR_PARTITIONING, TV_WHOPR_LTRANS, TV_IPA_REFERENCE, TV_IPA_PROFILE, TV_IPA_AUTOFDO, TV_IPA_PURE_CONST, TV_IPA_ICF, TV_IPA_PTA, TV_IPA_SRA, TV_IPA_FREE_LANG_DATA, TV_IPA_FREE_INLINE_SUMMARY, TV_IPA_MODREF, TV_CFG, TV_CLEANUP_CFG, TV_CFG_VERIFY, TV_DELETE_TRIVIALLY_DEAD, TV_DF_SCAN, TV_DF_MD, TV_DF_RD, TV_DF_LR, TV_DF_LIVE, TV_DF_MIR, TV_DF_CHAIN, TV_DF_WORD_LR, TV_DF_NOTE, TV_REG_STATS, TV_ALIAS_ANALYSIS, TV_ALIAS_STMT_WALK, TV_REG_SCAN, TV_REBUILD_JUMP, TV_CPP, TV_LEX, TV_PARSE_GLOBAL, TV_PARSE_STRUCT, TV_PARSE_ENUM, TV_PARSE_FUNC, TV_PARSE_INLINE, TV_PARSE_INMETH, TV_TEMPLATE_INST, TV_CONSTEXPR, TV_CONSTRAINT_NORM, TV_CONSTRAINT_SAT, TV_CONSTRAINT_SUB, TV_MODULE_IMPORT, TV_MODULE_EXPORT, TV_MODULE_MAPPER, TV_FLATTEN_INLINING, TV_EARLY_INLINING, TV_INLINE_PARAMETERS, TV_INTEGRATION, TV_TREE_GIMPLIFY, TV_TREE_EH, TV_TREE_CFG, TV_TREE_CLEANUP_CFG, TV_TREE_TAIL_MERGE, TV_TREE_VRP, TV_TREE_VRP_THREADER, TV_TREE_EARLY_VRP, TV_TREE_FAST_VRP, TV_TREE_ARRAY_BOUNDS, TV_TREE_COPY_PROP, TV_FIND_REFERENCED_VARS, TV_TREE_PTA, TV_TREE_SSA_OTHER, TV_TREE_INTO_SSA, TV_TREE_SSA_INCREMENTAL, TV_TREE_OPS, TV_TREE_SSA_DOMINATOR_OPTS, TV_TREE_SSA_THREAD_JUMPS, TV_TREE_SRA, TV_ISOLATE_ERRONEOUS_PATHS, TV_TREE_CCP, TV_TREE_SPLIT_EDGES, TV_TREE_REASSOC, TV_TREE_PRE, TV_TREE_FRE, TV_TREE_RPO_VN, TV_TREE_SINK, TV_TREE_PHIOPT, TV_TREE_BACKPROP, TV_TREE_FORWPROP, TV_TREE_PHIPROP, TV_TREE_DCE, TV_TREE_CD_DCE, TV_TREE_CALL_CDCE, TV_TREE_DSE, TV_TREE_MERGE_PHI, TV_TREE_LOOP, TV_TREE_NOLOOP, TV_TREE_LOOP_BOUNDS, TV_LIM, TV_LINTERCHANGE, TV_TREE_LOOP_IVCANON, TV_SCEV_CONST, TV_TREE_LOOP_UNSWITCH, TV_LOOP_SPLIT, TV_LOOP_JAM, TV_COMPLETE_UNROLL, TV_SCALAR_CLEANUP, TV_TREE_PARALLELIZE_LOOPS, TV_TREE_VECTORIZATION, TV_TREE_SLP_VECTORIZATION, TV_GRAPHITE, TV_GRAPHITE_TRANSFORMS, TV_GRAPHITE_DATA_DEPS, TV_GRAPHITE_CODE_GEN, TV_TREE_LOOP_DISTRIBUTION, TV_CHECK_DATA_DEPS, TV_TREE_PREFETCH, TV_TREE_LOOP_IVOPTS, TV_PREDCOM, TV_TREE_CH, TV_TREE_SSA_UNCPROP, TV_TREE_NRV, TV_TREE_COPY_RENAME, TV_TREE_SSA_VERIFY, TV_TREE_STMT_VERIFY, TV_TREE_SWITCH_CONVERSION, TV_TREE_SWITCH_LOWERING, TV_TREE_RECIP, TV_TREE_SINCOS, TV_TREE_POW, TV_TREE_WIDEN_MUL, TV_TRANS_MEM, TV_TREE_STRLEN, TV_TREE_MODREF, TV_TREE_ASSUMPTIONS, TV_CGRAPH_VERIFY, TV_DOM_FRONTIERS, TV_DOMINANCE, TV_CONTROL_DEPENDENCES, TV_OUT_OF_SSA, TV_VAR_EXPAND, TV_EXPAND, TV_POST_EXPAND, TV_VARCONST, TV_LOWER_SUBREG, TV_JUMP, TV_FWPROP, TV_CSE, TV_DCE, TV_DSE1, TV_DSE2, TV_LOOP, TV_LOOP_INIT, TV_LOOP_VERSIONING, TV_LOOP_MOVE_INVARIANTS, TV_LOOP_UNROLL, TV_LOOP_DOLOOP, TV_LOOP_FINI, TV_CPROP, TV_PRE, TV_HOIST, TV_LSM, TV_TRACER, TV_WEB, TV_AUTO_INC_DEC, TV_CSE2, TV_BRANCH_PROB, TV_COMBINE, TV_IFCVT, TV_MODE_SWITCH, TV_SMS, TV_LIVE_RANGE_SHRINKAGE, TV_SCHED, TV_EARLY_REMAT, TV_IRA, TV_LRA, TV_LRA_ELIMINATE, TV_LRA_INHERITANCE, TV_LRA_CREATE_LIVE_RANGES, TV_LRA_ASSIGN, TV_LRA_COALESCE, TV_LRA_REMAT, TV_RELOAD, TV_RELOAD_CSE_REGS, TV_GCSE_AFTER_RELOAD, TV_REE, TV_THREAD_PROLOGUE_AND_EPILOGUE, TV_IFCVT2, TV_SPLIT_PATHS, TV_COMBINE_STACK_ADJUST, TV_PEEPHOLE2, TV_RENAME_REGISTERS, TV_SCHED_FUSION, TV_CPROP_REGISTERS, TV_SCHED2, TV_MACH_DEP, TV_DBR_SCHED, TV_REORDER_BLOCKS, TV_SHORTEN_BRANCH, TV_REG_STACK, TV_FINAL, TV_VAROUT, TV_SYMOUT, TV_VAR_TRACKING, TV_VAR_TRACKING_DATAFLOW, TV_VAR_TRACKING_EMIT, TV_TREE_IFCOMBINE, TV_TREE_IF_TO_SWITCH, TV_TREE_UNINIT, TV_PLUGIN_INIT, TV_PLUGIN_RUN, TV_GIMPLE_SLSR, TV_GIMPLE_STORE_MERGING, TV_VTABLE_VERIFICATION, TV_TREE_UBSAN, TV_INITIALIZE_RTL, TV_GIMPLE_LADDRESS, TV_TREE_LOOP_IFCVT, TV_WARN_ACCESS, TV_EARLY_LOCAL, TV_OPTIMIZE, TV_REST_OF_COMPILATION, TV_POSTRELOAD, TV_LATE_COMPILATION, TV_REMOVE_UNUSED, TV_ADDRESS_TAKEN, TV_TODO, TV_VERIFY_LOOP_CLOSED, TV_VERIFY_RTL_SHARING, TV_REBUILD_FREQUENCIES, TV_REPAIR_LOOPS, TV_JIT_REPLAY, TV_ASSEMBLE, TV_LINK, TV_LOAD, TV_JIT_ACQUIRING_MUTEX, TV_JIT_CLIENT_CODE, TV_ANALYZER, TV_ANALYZER_SUPERGRAPH, TV_ANALYZER_STATE_PURGE, TV_ANALYZER_PLAN, TV_ANALYZER_SCC, TV_ANALYZER_WORKLIST, TV_ANALYZER_INFINITE_LOOPS, TV_ANALYZER_DUMP, TV_ANALYZER_DIAGNOSTICS, TV_ANALYZER_SHORTEST_PATHS, TIMEVAR_LAST }
        timevar_id_t;
        enum opt_pass_type {
      GIMPLE_PASS, RTL_PASS, SIMPLE_IPA_PASS, IPA_PASS };
        struct pass_data {
      enum opt_pass_type type;
      const char *name;
      optgroup_flags_t optinfo_flags;
      timevar_id_t tv_id;
      unsigned int properties_required;
      unsigned int properties_provided;
      unsigned int properties_destroyed;
      unsigned int todo_flags_start;
      unsigned int todo_flags_finish;
      };
        namespace gcc {
      class context;
      }
        class opt_pass : public pass_data {
      protected: opt_pass (const pass_data&, gcc::context *);
      };
        class ipa_opt_pass_d : public opt_pass {
      public: void (*generate_summary) (void);
      void (*write_summary) (void);
      void (*read_summary) (void);
      void (*write_optimization_summary) (void);
      void (*read_optimization_summary) (void);
      void (*stmt_fixup) (struct cgraph_node *, gimple **);
      unsigned int function_transform_todo_flags_start;
      unsigned int (*function_transform) (struct cgraph_node *);
      void (*variable_transform) (varpool_node *);
      protected: ipa_opt_pass_d (const pass_data& data, gcc::context *ctxt, void (*generate_summary) (void), void (*write_summary) (void), void (*read_summary) (void), void (*write_optimization_summary) (void), void (*read_optimization_summary) (void), void (*stmt_fixup) (struct cgraph_node *, gimple **), unsigned int function_transform_todo_flags_start, unsigned int (*function_transform) (struct cgraph_node *), void (*variable_transform) (varpool_node *)) : opt_pass (data, ctxt), generate_summary (generate_summary), write_summary (write_summary), read_summary (read_summary), write_optimization_summary (write_optimization_summary), read_optimization_summary (read_optimization_summary), stmt_fixup (stmt_fixup), function_transform_todo_flags_start (function_transform_todo_flags_start), function_transform (function_transform), variable_transform (variable_transform) {
    }
      };
        struct symtab_node {
      struct lto_file_decl_data * lto_file_data;
      };
        struct cgraph_node : public symtab_node {
      cgraph_edge *callees;
      cgraph_edge *indirect_calls;
      };
        class cgraph_edge {
      public: friend struct cgraph_node;
      inline int get_uid () {
    return m_uid;
    }
      cgraph_edge *next_callee;
      private: int m_uid;
      };
        template <class T> class function_summary;
        class symbol_table {
      };
        template <class T> class function_summary_base {
      };
        template <class T> class function_summary <T *>: public function_summary_base<T> {
      T* get (cgraph_node *node) __attribute__ ((__pure__)) {
    }
      };
        template <class T> class call_summary_base {
      };
        template <class T> class call_summary {
      };
        template <class T> class call_summary <T *>: public call_summary_base<T> {
      public: call_summary (symbol_table *symtab, bool ggc = false ) : call_summary_base<T> (symtab, call_summary::symtab_removal, call_summary::symtab_duplication ), m_ggc (ggc), m_map (13, ggc, true, 0 ) {
   }
      T* get_create (cgraph_edge *edge) {
    bool existed;
    T **v = &m_map.get_or_insert (edge->get_uid (), &existed);
    return *v;
    }
      protected: bool m_ggc;
      typedef int_hash <int, 0, -1> map_hash;
      hash_map <map_hash, T *> m_map;
      };
        enum lto_section_type {
      LTO_section_decls = 0, LTO_section_function_body, LTO_section_static_initializer, LTO_section_symtab, LTO_section_symtab_extension, LTO_section_refs, LTO_section_asm, LTO_section_jump_functions, LTO_section_ipa_pure_const, LTO_section_ipa_reference, LTO_section_ipa_profile, LTO_section_symtab_nodes, LTO_section_opts, LTO_section_cgraph_opt_sum, LTO_section_ipa_fn_summary, LTO_section_ipcp_transform, LTO_section_ipa_icf, LTO_section_offload_table, LTO_section_mode_table, LTO_section_lto, LTO_section_ipa_sra, LTO_section_odr_types, LTO_section_ipa_modref, LTO_N_SECTION_TYPES };
        class lto_input_block {
      public: lto_input_block (const char *data_, unsigned int p_, unsigned int len_, const lto_file_decl_data *file_data_) : data (data_), file_data (file_data_), p (p_), len (len_) {
   }
      lto_input_block (const char *data_, unsigned int len_, const lto_file_decl_data *file_data_) : data (data_), file_data (file_data_), p (0), len (len_) {
   }
      const char *data;
      const lto_file_decl_data *file_data;
      unsigned int p;
      unsigned int len;
      };
        struct lto_simple_header {
      int32_t main_size;
      };
        struct lto_simple_header_with_strings : lto_simple_header {
      };
        struct lto_function_header : lto_simple_header_with_strings {
      int32_t cfg_size;
      };
        typedef struct lto_symtab_encoder_d *lto_symtab_encoder_t;
        struct lto_symtab_encoder_iterator {
      };
        struct lto_out_decl_state {
      lto_symtab_encoder_t symtab_node_encoder;
      };
        struct output_block {
      struct lto_out_decl_state *decl_state;
      };
        extern struct lto_file_decl_data **lto_get_file_decl_data (void);
        extern const char *lto_get_summary_section_data (struct lto_file_decl_data *, enum lto_section_type, size_t *);
        extern struct output_block *create_output_block (enum lto_section_type);
        inline bool lsei_end_p (lto_symtab_encoder_iterator lsei) {
      }
        inline void lsei_next_function_in_partition (lto_symtab_encoder_iterator *lsei) {
      }
        inline lto_symtab_encoder_iterator lsei_start_function_in_partition (lto_symtab_encoder_t encoder) {
      }
        static unsigned const BITS_PER_BITPACK_WORD = 64;
        typedef unsigned long long bitpack_word_t;
        struct bitpack_d {
      unsigned pos;
      bitpack_word_t word;
      void *stream;
      };
        unsigned long long streamer_read_uhwi (class lto_input_block *);
        inline struct bitpack_d bitpack_create (struct lto_output_stream *s) {
      }
        inline struct bitpack_d streamer_read_bitpack (class lto_input_block *ib) {
      struct bitpack_d bp;
      return bp;
      }
        inline bitpack_word_t bp_unpack_value (struct bitpack_d *bp, unsigned nbits) {
      bitpack_word_t mask, val;
      int pos = bp->pos;
      mask = (nbits == BITS_PER_BITPACK_WORD ? (bitpack_word_t) -1 : ((bitpack_word_t) 1 << nbits) - 1);
      if (pos + nbits > BITS_PER_BITPACK_WORD) {
    bp->word = val = streamer_read_uhwi ((class lto_input_block *)bp->stream);
    bp->pos = nbits;
    return val & mask;
    }
      val = bp->word;
      val >>= pos;
      bp->pos = pos + nbits;
      return val & mask;
      }
        class ipcp_transformation;
        class ipa_argagg_value_list {
      template<typename pred_function> void remove_argaggs_if (pred_function &&predicate) {
    }
      };
        inline ipcp_transformation * ipcp_get_transformation_summary (cgraph_node *node) {
      }
        struct param_access {
      };
        struct isra_param_desc {
      vec <param_access *, va_gc> *accesses;
      unsigned remove_only_when_retval_removed : 1;
      };
        class isra_func_summary {
      public: isra_func_summary () : m_parameters (nullptr), m_candidate (false), m_returns_value (false), m_return_ignored (false), m_queued (false) {
   }
      vec<isra_param_desc, va_gc> *m_parameters;
      unsigned m_candidate : 1;
      unsigned m_returns_value : 1;
      unsigned m_return_ignored : 1;
      unsigned m_queued : 1;
      };
        struct isra_param_flow {
      char length;
      unsigned char inputs[7];
      unsigned aggregate_pass_through : 1;
      unsigned pointer_pass_through : 1;
      unsigned safe_to_import_accesses : 1;
      };
        class isra_call_summary {
      public: isra_call_summary () : m_arg_flow (), m_return_ignored (false), m_return_returned (false), m_bit_aligned_arg (false), m_before_any_store (false) {
   }
      auto_vec <isra_param_flow> m_arg_flow;
      unsigned m_return_ignored : 1;
      unsigned m_return_returned : 1;
      unsigned m_bit_aligned_arg : 1;
      unsigned m_before_any_store : 1;
      };
        class ipa_sra_function_summaries : public function_summary <isra_func_summary *> {
      };
        class ipa_sra_call_summaries: public call_summary <isra_call_summary *> {
      };
        static ipa_sra_call_summaries *call_sums;
        namespace {
      static void verify_splitting_accesses (cgraph_node *node, bool certain_must_exist) {
    }
      static void ipa_sra_generate_summary (void) {
    }
      static void isra_write_edge_summary (output_block *ob, cgraph_edge *e) {
    }
      static void isra_write_node_summary (output_block *ob, cgraph_node *node) {
    for (cgraph_edge *e = node->callees;
    e;
    e = e->next_callee) isra_write_edge_summary (ob, e);
    }
      static void ipa_sra_write_summary (void) {
    struct output_block *ob = create_output_block (LTO_section_ipa_sra);
    lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
    lto_symtab_encoder_iterator lsei;
    for (lsei = lsei_start_function_in_partition (encoder);
    !lsei_end_p (lsei);
    lsei_next_function_in_partition (&lsei)) {
  }
    }
      static void isra_read_edge_summary (struct lto_input_block *ib, cgraph_edge *cs) {
    isra_call_summary *csum = call_sums->get_create (cs);
    unsigned input_count = streamer_read_uhwi (ib);
    for (unsigned i = 0;
    i < input_count;
    i++) {
  isra_param_flow *ipf = &csum->m_arg_flow[i];
  bitpack_d bp = streamer_read_bitpack (ib);
  for (int j = 0;
  j < ipf->length;
  j++) ipf->inputs[j] = bp_unpack_value (&bp, 8);
  ipf->aggregate_pass_through = bp_unpack_value (&bp, 1);
  ipf->pointer_pass_through = bp_unpack_value (&bp, 1);
  ipf->safe_to_import_accesses = bp_unpack_value (&bp, 1);
  }
    }
      static void isra_read_node_info (struct lto_input_block *ib, cgraph_node *node, struct data_in *data_in) {
    for (cgraph_edge *e = node->indirect_calls;
    e;
    e = e->next_callee) isra_read_edge_summary (ib, e);
    }
      static void isra_read_summary_section (struct lto_file_decl_data *file_data, const char *data, size_t len) {
    const struct lto_function_header *header = (const struct lto_function_header *) data;
    const int cfg_offset = sizeof (struct lto_function_header);
    const int main_offset = cfg_offset + header->cfg_size;
    struct data_in *data_in;
    unsigned int i;
    unsigned int count;
    lto_input_block ib_main ((const char *) data + main_offset, header->main_size, file_data);
    count = streamer_read_uhwi (&ib_main);
    for (i = 0;
    i < count;
    i++) {
  struct cgraph_node *node;
  isra_read_node_info (&ib_main, node, data_in);
  }
    }
      static void ipa_sra_read_summary (void) {
    struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
    struct lto_file_decl_data *file_data;
    unsigned int j = 0;
    while ((file_data = file_data_vec[j++])) {
  size_t len;
  const char *data = lto_get_summary_section_data (file_data, LTO_section_ipa_sra, &len);
  if (data) isra_read_summary_section (file_data, data, len);
  }
    }
      static bool all_callee_accesses_present_p (isra_param_desc *param_desc, isra_param_desc *arg_desc) {
    unsigned aclen = vec_safe_length (arg_desc->accesses);
    for (unsigned j = 0;
    j < aclen;
    j++) {
  }
    }
      enum acc_prop_kind {
   ACC_PROP_DONT, ACC_PROP_COPY, ACC_PROP_CERTAIN};
      static const char * pull_accesses_from_callee (cgraph_node *caller, isra_param_desc *param_desc, isra_param_desc *arg_desc, unsigned delta_offset, unsigned arg_size, bool *change_p) {
    unsigned aclen = vec_safe_length (arg_desc->accesses);
    for (unsigned j = 0;
    j < aclen;
    j++) {
  }
    }
      static bool adjust_parameter_descriptions (cgraph_node *node, isra_func_summary *ifs) {
    unsigned len = vec_safe_length (ifs->m_parameters);
    for (unsigned i = 0;
    i < len;
    i++) {
  isra_param_desc *desc = &(*ifs->m_parameters)[i];
  if (desc->remove_only_when_retval_removed && !ifs->m_return_ignored) {
  for (const param_access *pa : desc->accesses) {
  }
  }
  }
    }
      const pass_data pass_data_ipa_sra = {
    IPA_PASS, "sra", OPTGROUP_NONE, TV_IPA_SRA, 0, 0, 0, 0, ( (1 << 7) | (1 << 8) ), };
      class pass_ipa_sra : public ipa_opt_pass_d {
    public: pass_ipa_sra (gcc::context *ctxt) : ipa_opt_pass_d (pass_data_ipa_sra, ctxt, ipa_sra_generate_summary, ipa_sra_write_summary, ipa_sra_read_summary, nullptr , nullptr, nullptr, 0, nullptr, nullptr) {
 }
    };
      }
        ipa_opt_pass_d * make_pass_ipa_sra (gcc::context *ctxt) {
      return new pass_ipa_sra (ctxt);
      }


