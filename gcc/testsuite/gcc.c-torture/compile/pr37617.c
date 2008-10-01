typedef union
{
  char *string;
  double dval;
  float fval;
} yystype;
char *f(void)
{
  yystype tok;
  tok.dval = 0;
  return (tok.string);
}
char *f1(void)
{
  yystype tok;
  tok.fval = 0;
  return (tok.string);
}

