/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling2 -fsel-sched-pipelining" } */

extern "C" {
	typedef struct _IO_FILE FILE;
	extern int putc(int __c, FILE * __stream);
	extern int strcmp(__const char *__s1, __const char *__s2) throw()
	    __attribute__ ((__pure__)) __attribute__ ((__nonnull__(1, 2)));
} typedef union tree_node *tree;
struct gcc_options {
	int x_flag_openmp;
};
extern struct gcc_options global_options;
struct ht_identifier {
	const unsigned char *str;
};
enum cpp_ttype {
	CPP_SEMICOLON, CPP_NAME
};
struct vl_embed {
};
struct va_heap {
};
struct va_gc {
	typedef vl_embed default_layout;
};
template < typename T, typename A = va_heap, typename L =
    typename A::default_layout > struct vec {
};
enum tree_code {
	ERROR_MARK,
	IDENTIFIER_NODE,
	OMP_SIMD,
	CILK_SIMD,
	MAX_TREE_CODES
};
struct tree_identifier {
	struct ht_identifier
	 id;
};
union tree_node {
	struct tree_identifier
	 identifier;
};
inline tree
tree_check(tree __t, const char *__f, int __l, const char *__g, tree_code __c)
{
}

extern tree chainon(tree, tree);
extern vec < tree, va_gc > *make_tree_vector(void);
typedef unsigned long omp_clause_mask;
enum c_omp_clause_split {
	C_OMP_CLAUSE_SPLIT_TARGET = 0, C_OMP_CLAUSE_SPLIT_COUNT
};
typedef struct cxx_saved_binding {
	tree attributes;
} cp_decl_specifier_seq;
typedef enum pragma_kind {
	PRAGMA_NONE = 0, PRAGMA_OMP_DECLARE_REDUCTION, PRAGMA_OMP_TARGET
} pragma_kind;
typedef enum pragma_omp_clause {
	PRAGMA_OMP_CLAUSE_NONE =
	    0, PRAGMA_OMP_CLAUSE_DEVICE, PRAGMA_OMP_CLAUSE_IF,
	    PRAGMA_OMP_CLAUSE_MAP
} pragma_omp_clause;
typedef struct cp_token {
	enum cpp_ttype type:8;
	union cp_token_value {
		tree value;
	} u;
} cp_token;
typedef struct cp_token *cp_token_position;
typedef struct cp_lexer {
	cp_token_position next_token;
	bool debugging_p;
	cp_lexer *lexer;
} cp_parser;
static FILE *cp_lexer_debug_stream;
static inline bool cp_lexer_debugging_p(cp_lexer * lexer)
{
	return lexer->debugging_p;
}

static inline cp_token *cp_lexer_peek_token(cp_lexer * lexer)
{
	if (cp_lexer_debugging_p(lexer)) {
		putc('\n', cp_lexer_debug_stream);
	}
	return lexer->next_token;
}

static inline bool cp_lexer_next_token_is(cp_lexer * lexer, enum cpp_ttype type)
{
}

enum {
	CP_PARSER_FLAGS_NONE = 0x0, CP_PARSER_FLAGS_OPTIONAL =
	    0x1, CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES =
	    0x2, CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS =
	    0x4, CP_PARSER_FLAGS_ONLY_TYPE_OR_CONSTEXPR = 0x8
};
typedef int cp_parser_flags;
extern tree
cp_parser_type_specifier(cp_parser *, cp_parser_flags,
			 cp_decl_specifier_seq *, bool, int *, bool *);
static void
cp_parser_type_specifier_seq(cp_parser *, bool, bool, cp_decl_specifier_seq *);
extern bool cp_next_tokens_can_be_attribute_p(cp_parser *);
extern tree cp_parser_attributes_opt(cp_parser *);
enum pragma_context {
	pragma_external,
	pragma_member,
	pragma_objc_icode,
	pragma_stmt,
	pragma_compound
};
static bool cp_parser_pragma(cp_parser *, enum pragma_context);
static bool cp_parser_translation_unit(cp_parser * parser)
{
	while (true) {
		cp_token *token;
		if (token->type == CPP_SEMICOLON) {
			cp_parser_pragma(parser, pragma_external);
		}
	}
}

static tree
cp_parser_type_id_1(cp_parser * parser, bool is_template_arg,
		    bool is_trailing_return)
{
	cp_decl_specifier_seq type_specifier_seq;
	cp_parser_type_specifier_seq(parser, false, is_trailing_return,
				     &type_specifier_seq);
}

static tree cp_parser_type_id(cp_parser * parser)
{
	return cp_parser_type_id_1(parser, false, false);
}

static void
cp_parser_type_specifier_seq(cp_parser * parser, bool is_declaration,
			     bool is_trailing_return,
			     cp_decl_specifier_seq * type_specifier_seq)
{
	cp_parser_flags flags = CP_PARSER_FLAGS_OPTIONAL;
	cp_token *start_token = __null;
	while (true) {
		tree type_specifier;
		bool is_cv_qualifier;
		if (cp_next_tokens_can_be_attribute_p(parser)) {
			type_specifier_seq->attributes =
			    chainon(type_specifier_seq->attributes,
				    cp_parser_attributes_opt(parser));
			continue;
		}
		if (!start_token)
			start_token = cp_lexer_peek_token(parser->lexer);
		type_specifier =
		    cp_parser_type_specifier(parser, flags, type_specifier_seq,
					     false, __null, &is_cv_qualifier);
		if (!type_specifier) {
			break;
		}
		if (is_declaration && !is_cv_qualifier)
			flags |= CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES;
	}
}

static bool
cp_parser_omp_for_loop_init(cp_parser * parser, bool parsing_openmp,
			    tree & this_pre_body, vec < tree,
			    va_gc > *for_block, tree & init, tree & decl,
			    tree & real_decl)
{
	cp_decl_specifier_seq type_specifiers;
	cp_parser_type_specifier_seq(parser, true, false, &type_specifiers);
}

static tree
cp_parser_omp_for_loop(cp_parser * parser, enum tree_code code, tree clauses,
		       tree * cclauses)
{
	tree init, cond, incr, body, decl, pre_body = (tree) __null, ret;
	tree real_decl, initv, condv, incrv, declv;
	tree this_pre_body, cl;
	int i, collapse = 1, nbraces = 0;
	vec < tree, va_gc > *for_block = make_tree_vector();
	for (i = 0; i < collapse; i++) {
		bool add_private_clause = false;
		add_private_clause |=
		    cp_parser_omp_for_loop_init(parser, code != CILK_SIMD,
						this_pre_body, for_block, init,
						decl, real_decl);
	}
}

static tree
cp_parser_omp_simd(cp_parser * parser, cp_token * pragma_tok, char *p_name,
		   omp_clause_mask mask, tree * cclauses)
{
	tree clauses, sb, ret;
	ret = cp_parser_omp_for_loop(parser, OMP_SIMD, clauses, cclauses);
}

static tree
cp_parser_omp_distribute(cp_parser * parser, cp_token * pragma_tok,
			 char *p_name, omp_clause_mask mask, tree * cclauses)
{
	if (cp_lexer_next_token_is(parser->lexer, CPP_NAME)) {
		tree id = cp_lexer_peek_token(parser->lexer)->u.value;
		const char *p =
		    ((const char
		      *)(tree_check((id),
				    "/home/bonzo/develop/trunk/gcc/cp/parser.c",
				    29966, __FUNCTION__,
				    (IDENTIFIER_NODE)))->identifier.id.str);
		bool simd = false;
		bool parallel = false;
		if (strcmp(p, "simd") == 0)
			simd = true;
		if (parallel || simd) {
			if (!global_options.x_flag_openmp) {
				if (simd)
					return cp_parser_omp_simd(parser,
								  pragma_tok,
								  p_name, mask,
								  cclauses);
			}
		}
	}
}

static tree
cp_parser_omp_teams(cp_parser * parser, cp_token * pragma_tok, char *p_name,
		    omp_clause_mask mask, tree * cclauses)
{
	if (cp_lexer_next_token_is(parser->lexer, CPP_NAME)) {
		tree id = cp_lexer_peek_token(parser->lexer)->u.value;
		const char *p =
		    ((const char
		      *)(tree_check((id),
				    "/home/bonzo/develop/trunk/gcc/cp/parser.c",
				    30062, __FUNCTION__,
				    (IDENTIFIER_NODE)))->identifier.id.str);
		if (strcmp(p, "distribute") == 0) {
			if (!global_options.x_flag_openmp)
				return cp_parser_omp_distribute(parser,
								pragma_tok,
								p_name, mask,
								cclauses);
		}
	}
}

static bool
cp_parser_omp_target(cp_parser * parser, cp_token * pragma_tok,
		     enum pragma_context context)
{
	if (context != pragma_stmt && context != pragma_compound) {
		tree id = cp_lexer_peek_token(parser->lexer)->u.value;
		const char *p =
		    ((const char
		      *)(tree_check((id),
				    "/home/bonzo/develop/trunk/gcc/cp/parser.c",
				    30201, __FUNCTION__,
				    (IDENTIFIER_NODE)))->identifier.id.str);
		if (strcmp(p, "teams") == 0) {
			tree cclauses[C_OMP_CLAUSE_SPLIT_COUNT];
			char p_name[sizeof
				    ("#pragma omp target teams distribute "
				     "parallel for simd")];
			if (!global_options.x_flag_openmp)
				return cp_parser_omp_teams(parser, pragma_tok,
							   p_name,
							   ((((omp_clause_mask)
							      1) <<
							     PRAGMA_OMP_CLAUSE_DEVICE)
							    |
							    (((omp_clause_mask)
							      1) <<
							     PRAGMA_OMP_CLAUSE_MAP)
							    |
							    (((omp_clause_mask)
							      1) <<
							     PRAGMA_OMP_CLAUSE_IF)),
							   cclauses);
		}
	}
}

static void
cp_parser_omp_declare_reduction(cp_parser * parser, cp_token * pragma_tok,
				enum pragma_context)
{
	tree reduc_id = (tree) __null, orig_reduc_id = (tree) __null, type;
	while (true) {
		type = cp_parser_type_id(parser);
	}
}

static void
cp_parser_omp_declare(cp_parser * parser, cp_token * pragma_tok,
		      enum pragma_context context)
{
	if (cp_lexer_next_token_is(parser->lexer, CPP_NAME)) {
		tree id = cp_lexer_peek_token(parser->lexer)->u.value;
		const char *p =
		    ((const char
		      *)(tree_check((id),
				    "/home/bonzo/develop/trunk/gcc/cp/parser.c",
				    30883, __FUNCTION__,
				    (IDENTIFIER_NODE)))->identifier.id.str);
		if (strcmp(p, "simd") == 0) {
			cp_parser_omp_declare_reduction(parser, pragma_tok,
							context);
		}
	}
}

static cp_parser *the_parser;
static bool cp_parser_pragma(cp_parser * parser, enum pragma_context context)
{
	cp_token *pragma_tok;
	unsigned int id;
	switch (id) {
	case PRAGMA_OMP_DECLARE_REDUCTION:
		cp_parser_omp_declare(parser, pragma_tok, context);
	case PRAGMA_OMP_TARGET:
		return cp_parser_omp_target(parser, pragma_tok, context);
	}
}

void c_parse_file(void)
{
	cp_parser_translation_unit(the_parser);
}
