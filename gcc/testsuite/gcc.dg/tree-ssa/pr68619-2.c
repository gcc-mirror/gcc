/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2-details -w" } */

typedef union tree_node *tree;
struct gcc_options
{
  int x_flag_finite_math_only;
};
extern struct gcc_options global_options;
enum mode_class
{ MODE_RANDOM, MODE_CC, MODE_INT, MODE_PARTIAL_INT, MODE_FRACT, MODE_UFRACT,
  MODE_ACCUM, MODE_UACCUM, MODE_FLOAT, MODE_DECIMAL_FLOAT, MODE_COMPLEX_INT,
  MODE_COMPLEX_FLOAT, MODE_VECTOR_INT, MODE_VECTOR_FRACT,
  MODE_VECTOR_UFRACT, MODE_VECTOR_ACCUM, MODE_VECTOR_UACCUM,
  MODE_VECTOR_FLOAT, MAX_MODE_CLASS
};
extern const unsigned char mode_class[27];
extern const unsigned char mode_inner[27];
struct real_value
{
};
struct real_format
{
  unsigned char has_inf;
};
extern const struct real_format *real_format_for_mode[5 -
						      2 + 1 + 15 - 10 + 1];
struct tree_type
{
};
union tree_node
{
  int code;
  int mode;
  struct tree_type type;
};
tree
omp_reduction_init (tree clause, tree type)
{
  if ((((type)->code) == 64))
    {
      struct real_value max;
      if (((((mode_class[((((type))->code) ==
			  32 ?
			  vector_type_mode (type)
			  : (type)->mode)]) ==
	     MODE_VECTOR_FLOAT)
	    &&
	    ((real_format_for_mode
	      [((mode_class[((mode_class[((((type))->code) ==
					  32 ?
					  vector_type_mode (type)
					  : (type)->mode)]) ==
			     12) ? (((((type))->code)
				     ==
				     32 ?
				     vector_type_mode
				     (type)
				     : (type)->mode))
			    : (mode_inner[((((type))->code) ==
					   32 ?
					   vector_type_mode (type)
					   : (type)->mode)])]) ==
		12)
	       ? (((((mode_class[((((type))->code) ==
				  32 ? vector_type_mode (type)
				  : (type)->mode)]) ==
		     12) ? (((((type))->code) ==
			     32 ?
			     vector_type_mode (type)
			     : (type)->mode)) : (mode_inner
						 [((((type))->code) ==
						   32 ?
						   vector_type_mode (type)
						   : (type)->mode)])) - 10) +
		  (5 - 2 +
		   1))
	       : ((((mode_class
		     [((((type))->code) ==
		       32 ? vector_type_mode (type) : (type)->mode)]) ==
		    12) ? (((((type))->code) ==
			    32 ? vector_type_mode (type) : (type)->
			    mode)) : (mode_inner[((((type))->code) ==
						  32 ? vector_type_mode (type)
						  : (type)->mode)])) -
		  2)]))->has_inf) && !global_options.x_flag_finite_math_only))
	real_inf (&max);
    }
}

/* { dg-final { scan-tree-dump "Marking all outgoing edges of unreachable" "dom2"} } */

