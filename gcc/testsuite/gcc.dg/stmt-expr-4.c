
/* { dg-options "-O2 -std=gnu99" } */
/* Internal compiler error in iterative_hash_expr */

struct tree_string
{
  char str[1];
};

union tree_node
{
  struct tree_string string;
};

char *Foo (union tree_node * num_string)
{
  char *str = ((union {const char * _q; char * _nq;})
	       ((const char *)(({ __typeof (num_string) const __t
				     = num_string;  __t; })
			       ->string.str)))._nq;
  return str;
}
