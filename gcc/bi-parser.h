typedef union
{
  char *string;
  struct def *def;
  struct variation *variation;
  struct node *node;
} YYSTYPE;
#define	DEFOP	258
#define	STRING	259


extern YYSTYPE yylval;
