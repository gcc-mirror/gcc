/* { dg-do compile } */

extern char *globerr;
char **ftpglob();

static const int yypgoto[] =
{
     -82, -82, -82, -82
};

static const int yydefgoto[] =
{
       0, 1, 36, 37
};

static const int yytable[] =
{
      43, 129, 88, 89
};

static const int yycheck[] =
{
       8, 82, 4, 5
};


int yyparse (char **yyvsp, char *yyvsp1)
{
    int yystate = 0;

  int yyn;
  int yyresult;
  int yyval;

yyreduce:

  switch (yyn)
    {
  case 72: {

  if (__builtin_strncmp( yyvsp[0], "~", 1) == 0) {
   *(char **)&(yyval) = *ftpglob(yyvsp[0]);
   if (globerr != 0) {
    yyval = 0;
   }
   __builtin_free(yyvsp[0]);
  }
 }
    break;
    }

  *++yyvsp1 = yyval;

  {
    const int yyi = yypgoto[0] + *yyvsp1;
    yystate = (yycheck[yyi] == *yyvsp1 ? 0 : 0);
  }

  return yyresult;
}

