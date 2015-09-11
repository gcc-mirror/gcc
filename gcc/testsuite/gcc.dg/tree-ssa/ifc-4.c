/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize -fdump-tree-ifcvt-stats" { target *-*-* } } */

struct ht
{
  void * (*alloc_subobject) (int);
};
typedef struct cpp_reader cpp_reader;
typedef struct cpp_token cpp_token;
typedef struct cpp_macro cpp_macro;
enum cpp_ttype
{
    CPP_PASTE,
};
struct cpp_token {
  __extension__ enum cpp_ttype type : 8;
} cpp_comment_table;
struct cpp_macro {
  union cpp_macro_u
  {
    cpp_token * tokens;
  } exp;
  unsigned int count;
};
struct cpp_reader
{
  struct ht *hash_table;
};
void
create_iso_definition (cpp_reader *pfile, cpp_macro *macro)
{
  unsigned int num_extra_tokens = 0;
  {
    cpp_token *tokns =
      (cpp_token *) pfile->hash_table->alloc_subobject (sizeof (cpp_token)
							* macro->count);
    {
      cpp_token *normal_dest = tokns;
      cpp_token *extra_dest = tokns + macro->count - num_extra_tokens;
      unsigned int i;
      for (i = 0; i < macro->count; i++)
	{
	  if (macro->exp.tokens[i].type == CPP_PASTE)
	    *extra_dest++ = macro->exp.tokens[i];
	  else
	    *normal_dest++ = macro->exp.tokens[i];
	}
    }
  }
}

/* This cannot be if-converted because the stores are to aggregate types.  */
/* { dg-final { scan-tree-dump-times "Applying if-conversion" 0 "ifcvt" } } */
