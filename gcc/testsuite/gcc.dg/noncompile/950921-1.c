typedef enum
{
  a = (X) 0,	/* { dg-error "undeclared|not integer|(parse|syntax) error" } */
  b
} c;

typedef enum
{
  d = (X) 0	/* { dg-error "undeclared|not integer|(parse|syntax) error" } */
} e;
