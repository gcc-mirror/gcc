/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc -w" } */

typedef union tree_node *tree;
enum cpp_ttype { CPP_COLON, CPP_SEMICOLON, CPP_CLOSE_BRACE, CPP_COMMA };
enum rid { RID_STATIC = 0, RID_ATTRIBUTE, };
typedef struct c_token
{
  enum cpp_ttype type:8;
}
c_token;
typedef struct c_parser
{
  c_token tokens[2];
  short tokens_avail;
}
c_parser;
__inline__ c_token *
c_parser_peek_token (c_parser * parser)
{
  if (parser->tokens_avail == 0)
    {
      parser->tokens_avail = 1;
    }
  return &parser->tokens[0];
}

__inline__ unsigned char
c_parser_next_token_is (c_parser * parser, enum cpp_ttype type)
{
  return c_parser_peek_token (parser)->type == type;
}

void
c_parser_translation_unit (c_parser * parser)
{
  tree prefix_attrs;
  tree all_prefix_attrs;
  while (1)
    {
      if (c_parser_next_token_is (parser, CPP_COLON)
	  || c_parser_next_token_is (parser, CPP_COMMA)
	  || c_parser_next_token_is (parser, CPP_SEMICOLON)
	  || c_parser_next_token_is (parser, CPP_CLOSE_BRACE)
	  || c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	{
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    all_prefix_attrs =
	      chainon (c_parser_attributes (parser), prefix_attrs);
	}
    }
}
/* { dg-final { scan-tree-dump-not "0 != 0" "reassoc2"} } */
