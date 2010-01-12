typedef __SIZE_TYPE__ size_t;
void *malloc(size_t);
typedef union YYSTYPE {
    char *id;
}  YYSTYPE;
extern YYSTYPE yylval;
void yylex (int b)
{
  yylval = (YYSTYPE) (b ? 0 : (char *) malloc (4));
}
