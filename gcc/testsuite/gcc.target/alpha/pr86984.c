/* { dg-do compile } */
/* { dg-options "-Wall -Wwrite-strings -Werror -fmerge-all-constants -fno-stack-protector -mieee -fdump-rtl-expand" } */

struct expression {
  unsigned long int num;
};
union YYSTYPE {
  unsigned long int num;
  struct expression *exp;
};

typedef union YYSTYPE YYSTYPE;

struct expression * new_exp_0 (int);

union yyalloc {
  short yyss_alloc;
};

static const signed char yypact[] = {
  -9, -9, -10, -10, -9, 8, 36, -10, 13, -10, -9, -9, -9, -9, -9, -9, -9, -10, 26, 41, 45, 18, -2, 14, -10, -9, 36 };
static const unsigned char yydefact[] = {
  0, 0, 12, 11, 0, 0, 2, 10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 13, 0, 4, 5, 6, 7, 8, 9, 0, 3 };

static const signed char yypgoto[3] = "\366\366\377";
static const signed char yydefgoto[3] = "\377\005\006";

static const unsigned char yytable[] = {
  7, 1, 2, 8, 3, 4, 15, 16, 9, 18, 19, 20, 21, 22, 23, 24, 10, 11, 12, 13, 14, 15, 16, 16, 26, 14, 15, 16, 17, 10, 11, 12, 13, 14, 15, 16, 0, 0, 25, 10, 11, 12, 13, 14, 15, 16, 12, 13, 14, 15, 16, 13, 14, 15, 16 };

static const signed char yycheck[] = {
  1, 10, 11, 4, 13, 14, 8, 9, 0, 10, 11, 12, 13, 14, 15, 16, 3, 4, 5, 6, 7, 8, 9, 9, 25, 7, 8, 9, 15, 3, 4, 5, 6, 7, 8, 9, -1, -1, 12, 3, 4, 5, 6, 7, 8, 9, 5, 6, 7, 8, 9, 6, 7, 8, 9 };

static const unsigned char yyr1[] = {
  0, 16, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18 };

static const unsigned char yyr2[] = {
  0, 2, 1, 5, 3, 3, 3, 3, 3, 3, 2, 1, 1, 3 };

int __gettextparse (void)
{
  int yystate = 0;
  short yyssa[200];
  short *yyss = yyss;
  short *yyssp = yyssa;
  YYSTYPE yyvsa[200];
  YYSTYPE *yyvsp = yyvsa;
  enum { yystacksize = 200 };
  int yylen = 0;
  goto yysetstate;
 yynewstate: yyssp++;
 yysetstate: *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      long unsigned int yysize = yyssp - yyss + 1;
      {
	short *yyss1 = yyss;
	union yyalloc *yyptr = (union yyalloc *) __builtin_malloc ((yystacksize * (sizeof (short) + sizeof (YYSTYPE)) + (sizeof (union yyalloc) - 1)));
	if (!yyptr) return 0;
	__builtin_memcpy (&yyptr->yyss_alloc, yyss, yysize * sizeof *(yyss));
	yyss = &yyptr->yyss_alloc;
	if (yyss1 != yyssa) __builtin_free (yyss1);
      }
      if (yyss + yystacksize - 1 <= yyssp)
	return 0;
    }

  int yyn = yypact[yystate];
  if (yyn == -10)
    goto yydefault;

  yyn = yytable[yyn];
  if (yyn <= 0)
    goto yyreduce;

 yydefault: yyn = yydefact[yystate];
 yyreduce: yylen = yyr2[yyn];

  YYSTYPE yyval;
  if (yyn == 12 && (yyval.exp = new_exp_0 (0)) != 0)
    (yyval.exp)->num = (yyvsp[0].num);

  (yyvsp -= yylen, yyssp -= yylen);
  yyn = yyr1[yyn];
  yystate = yypgoto[yyn - 16] + *yyssp;
  if (0 <= yystate && yystate <= 54 && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - 16];

  goto yynewstate;
}

/* { dg-final { scan-rtl-dump-not "const_int 230584300921" "expand" } } */
/* { dg-final { scan-assembler-not "yypgoto\\+230584300921" } } */
