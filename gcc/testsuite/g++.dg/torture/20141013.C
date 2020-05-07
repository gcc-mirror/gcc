/* { dg-options "-fno-short-enums -Wno-return-type" } */
enum
{
  _sch_isdigit = 0x0004,
  _sch_ispunct = 0x0020,
  _sch_isxdigit = 0x0100,
  _sch_isidst = 0x0200,
  _sch_isvsp = 0x0400,
  _sch_isnvsp = 0x0800,
  _sch_isalnum = _sch_isidst | _sch_isdigit,
  _sch_iscppsp = _sch_isvsp | _sch_isnvsp,
};
extern const unsigned short _sch_istable[256];
typedef union tree_node *tree;
typedef const union tree_node *const_tree;
enum opt_code
{
  OPT_Warray_bounds = 240,
  OPT_Wformat_ = 245,
  OPT_Wintf_annotation = 368,
  OPT_std_gnu__14 = 1311,
};
enum tree_code
{
  TREE_LIST,
  CONST_DECL,
  ADDR_EXPR,
  MAX_TREE_CODES
};
enum tree_code_class
{
  tcc_type,
};
enum tree_node_structure_enum
{
  TS_TYPED,
  TS_COMMON,
};
enum integer_type_kind
{
  itk_char,
  itk_none
};
struct tree_base
{
  enum tree_code code:16;
};
struct tree_typed
{
  tree type;
};
struct tree_common
{
  tree chain;
};
struct tree_list
{
  tree purpose;
};
struct tree_type_common
{
  tree main_variant;
};
union tree_node
{
  struct tree_base base;
  struct tree_typed typed;
  struct tree_common common;
  struct tree_type_common type_common;
  struct tree_list list;
};
extern unsigned char tree_contains_struct[MAX_TREE_CODES][64];
extern tree integer_types[itk_none];
extern void tree_contains_struct_check_failed (const_tree,
					       tree_node_structure_enum,
					       const char *, int,
					       const char *)
  __attribute__ ((__noreturn__));
inline tree
tree_check (tree __t, const char *__f, int __l, const char *__g,
	    tree_code __c)
{
}

inline const_tree
contains_struct_check (const_tree __t,
		       const enum tree_node_structure_enum __s,
		       const char *__f, int __l, const char *__g)
{
  if (tree_contains_struct[((enum tree_code) (__t)->base.code)][__s] != 1)
    tree_contains_struct_check_failed (__t, __s, __f, __l, __g);
}

inline const_tree
tree_class_check (const_tree __t, const enum tree_code_class __class,
		  const char *__f, int __l, const char *__g)
{
}

static inline bool
is_attribute_p (const char *attr_name, const_tree ident)
{
}

extern int integer_zerop (const_tree);
extern bool warning (int, const char *, ...)
  __attribute__ ((__nonnull__ (2)));
extern void
check_function_arguments_recurse (void (*)(void *, tree, unsigned long long),
				  void *, tree, unsigned long long);
extern bool objc_string_ref_type_p (tree);
enum
{
  FMT_FLAG_SCANF_A_KLUDGE = 2,
  FMT_FLAG_PARSE_ARG_CONVERT_EXTERNAL = 256
};
typedef struct
{
}
format_flag_spec;
typedef struct
{
  int flags;
  tree *width_type;
}
format_kind_info;
typedef struct alloc_pool_list_def
{
}
 *alloc_pool;
struct gcc_targetcm
{
  bool (*string_object_ref_type_p) (const_tree stringref);
}
 ;
extern struct gcc_targetcm targetcm;
enum format_type
{
  gcc_objc_string_format_type,
};
typedef struct function_format_info
{
  int format_type;
}
function_format_info;
static const format_kind_info format_types_orig[] = { };
struct format_check_context { };

static const format_kind_info *format_types = format_types_orig;
static void check_format_info (function_format_info *, tree);
void check_format_arg (void *, tree, unsigned long long);

void
check_function_format (tree attrs, int nargs, tree * argarray)
{
  tree a;
  for (a = attrs;
       a;
       ((contains_struct_check
	 ((a), (TS_COMMON), "../../git-master/gcc/c-family/c-format.c", 1002,
	  __FUNCTION__))->common.chain))
    {
      if (is_attribute_p
	  ("format",
	   ((tree_check
	     ((a), "../../git-master/gcc/c-family/c-format.c", 1004,
	      __FUNCTION__, (TREE_LIST)))->list.purpose)))
	{
	  function_format_info info;
	  {
	    tree params = (tree) __null;
	    check_format_info (&info, params);
	  }
	}
    }
}

static bool
avoid_dollar_number (const char *format)
{
  while ((_sch_istable[(*format) & 0xff] & (unsigned short) (_sch_isdigit)))
    format++;
  if (*format == '$')
    {
      warning (OPT_Wformat_,
	       "$ operand number used after format without operand number");
    }
}

static void
check_format_info (function_format_info * info, tree params)
{
  format_check_context format_ctx;
  unsigned long long arg_num;
  tree format_tree;
  check_function_arguments_recurse (check_format_arg, &format_ctx,
				    format_tree, arg_num);
  const char *format_chars;
  if (integer_zerop (format_tree))
    {
      {
	((contains_struct_check
	  ((params), (TS_COMMON),
	   "../../git-master/gcc/c-family/c-format.c", 1444,
	   __FUNCTION__))->common.chain);
      }
      return;
    }
  if (((enum tree_code) (format_tree)->base.code) != ADDR_EXPR)
    {
      return;
    }
  if (format_types[info->format_type].flags & (int)
      FMT_FLAG_PARSE_ARG_CONVERT_EXTERNAL)
    {
      bool objc_str = (info->format_type == gcc_objc_string_format_type);
      if (((enum tree_code) (format_tree)->base.code) != CONST_DECL
	  ||
	  !((objc_str
	     &&
	     objc_string_ref_type_p (((contains_struct_check
				       ((format_tree), (TS_TYPED),
					"../../git-master/gcc/c-family/c-format.c",
					1498, __FUNCTION__))->typed.type)))
	    ||
	    (*targetcm.string_object_ref_type_p) ((const_tree)
						  ((contains_struct_check
						    ((format_tree),
						     (TS_TYPED),
						     "../../git-master/gcc/c-family/c-format.c",
						     1500,
						     __FUNCTION__))->typed.
						   type))))
	{
	}
    }
  {
  }
  if (((tree_class_check
	((((contains_struct_check
	    ((((contains_struct_check
		((format_tree), (TS_TYPED),
		 "../../git-master/gcc/c-family/c-format.c", 1549,
		 __FUNCTION__))->typed.type)), (TS_TYPED),
	     "../../git-master/gcc/c-family/c-format.c", 1549,
	     __FUNCTION__))->typed.type)), (tcc_type),
	 "../../git-master/gcc/c-family/c-format.c", 1549,
	 __FUNCTION__))->type_common.main_variant) != integer_types[itk_char])
    {
      return;
    }
  {
  }
  const format_kind_info *fki = &format_types[info->format_type];
  while (*format_chars != 0)
    {
      {
	if (fki->width_type != __null && *format_chars == '*')
	  {
	    {
	      if (avoid_dollar_number (format_chars))
		if (avoid_dollar_number (format_chars))
		  return;
	    }
	  }
      }
    }
}
