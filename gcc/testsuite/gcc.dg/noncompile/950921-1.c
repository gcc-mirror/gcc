typedef enum
{
  a = (X) 0,	/* { dg-error "undeclared|not integer|parse error|syntax error|expected" } */
  b
} c;

typedef enum
{
  d = (X) 0	/* { dg-error "undeclared|not integer|parse error|syntax error|expected" } */
} e;
