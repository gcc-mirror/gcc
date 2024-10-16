/* Inspired by part of java/parse.y.
   May be a real bug in CSE. */

/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-old-style-definition" } */

struct tree
{
    struct tree *car, *cdr, *wfl;
    int code;
    struct { unsigned int renp:1;
      unsigned int rtnp:1;
      unsigned int rpnp:1; } flags;
};
typedef struct tree *tree;
#define NULL_TREE ((tree)0)

/* Codes */
enum
{
    CALL_EXPR, NEW_ARRAY_EXPR, NEW_CLASS_EXPR, CONVERT_EXPR,
    ARRAY_REF, CONDITIONAL_EXPR, STRING_CST, EXPR_WITH_FILE_LOCATION
};

/* Flags */
#define RESOLVE_EXPRESSION_NAME_P(t) ((t)->flags.renp)
#define RESOLVE_TYPE_NAME_P(t) ((t)->flags.rtnp)
#define RESOLVE_PACKAGE_NAME_P(t) ((t)->flags.rpnp)

/* Macros */
#define EXPR_WFL_QUALIFICATION(t) ((t)->wfl)
#define QUAL_WFL(t) ((t)->wfl)
#define EXPR_WFL_NODE(t) ((t)->wfl)
#define TREE_CODE(t) ((t)->code)
#define TREE_OPERAND(t,x) ((t)->car)
#define CLASSTYPE_SUPER(t) ((t)->car)
#define IDENTIFIER_LOCAL_VALUE(t) ((t)->car)
#define TREE_CHAIN(t) ((t)->cdr)
#define QUAL_RESOLUTION(t) ((t)->cdr)

extern tree current_class, this_identifier_node;
extern tree super_identifier_node, length_identifier_node;

tree resolve_and_layout (tree, tree);
tree lookup_field_wrapper (tree, tree);

void
qualify_ambiguous_name (id)
     tree id;
{
  tree qual, qual_wfl, decl;
  tree name;	 /* { dg-bogus "name" "uninitialized variable warning" } */
  tree ptr_type; /* { dg-bogus "ptr_type" "uninitialized variable warning" } */
  int again, new_array_found = 0;
  int super_found = 0, this_found = 0;

  qual = EXPR_WFL_QUALIFICATION (id);
  do {
    qual_wfl = QUAL_WFL (qual);
    switch (TREE_CODE (qual_wfl))
      {
      case CALL_EXPR:
	qual_wfl = TREE_OPERAND (qual_wfl, 0);
	if (TREE_CODE (qual_wfl) != EXPR_WITH_FILE_LOCATION)
	  {
	    qual = EXPR_WFL_QUALIFICATION (qual_wfl);
	    qual_wfl = QUAL_WFL (qual);
	  }
	break;
      case NEW_ARRAY_EXPR:
	qual = TREE_CHAIN (qual);
	new_array_found = again = 1;
	continue;
      case NEW_CLASS_EXPR:
      case CONVERT_EXPR:
	qual_wfl = TREE_OPERAND (qual_wfl, 0);
	break;
      case ARRAY_REF:
	while (TREE_CODE (qual_wfl) == ARRAY_REF)
	  qual_wfl = TREE_OPERAND (qual_wfl, 0);
	break;
      default:
	break;
      }

    name = EXPR_WFL_NODE (qual_wfl);
    ptr_type = current_class;
    again = 0;

  } while (again);

  /* If you put straightforward uses of name and ptr_type here
     instead of the if-else sequence below, the warnings go away.
     Therefore I suspect a real bug. */
  
  if (!this_found && !super_found && (decl = IDENTIFIER_LOCAL_VALUE (name)))
    {
      RESOLVE_EXPRESSION_NAME_P (qual_wfl) = 1;
      QUAL_RESOLUTION (qual) = decl;
    }
  else if ((decl = lookup_field_wrapper (ptr_type, name))
	   || (new_array_found && name == length_identifier_node))
    {
      RESOLVE_EXPRESSION_NAME_P (qual_wfl) = 1;
      QUAL_RESOLUTION (qual) = (new_array_found ? NULL_TREE : decl);
    }
  else if ((decl = resolve_and_layout (name, NULL_TREE)))
    {
      RESOLVE_TYPE_NAME_P (qual_wfl) = 1;
      QUAL_RESOLUTION (qual) = decl;
    }
  else if (TREE_CODE (QUAL_WFL (qual)) == CALL_EXPR
	   || TREE_CODE (QUAL_WFL (qual)) == ARRAY_REF)
    RESOLVE_EXPRESSION_NAME_P (qual_wfl) = 1;
  else 
    RESOLVE_PACKAGE_NAME_P (qual_wfl) = 1;
}
