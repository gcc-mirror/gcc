typedef enum
{
  a = (X) 0,	/* { dg-error "undeclared|not integer|parse error" } */
  b
} c;

typedef enum
{
  d = (X) 0	/* { dg-error "undeclared|not integer|parse error" } */
} e;
