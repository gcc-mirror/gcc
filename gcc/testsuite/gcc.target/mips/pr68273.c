/* { dg-do compile } */
/* { dg-options "-w -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* }  { } { "-O2" } } */

extern char errbuf[];
typedef struct Symbol
{
  char *name;
}
Symbol;
typedef struct Tnode
{
}
Tnode;
typedef union Value
{
  long long i;
}
Value;
typedef struct Entry
{
}
Entry;
typedef struct Table
{
}
Table;
typedef struct Node
{
  Tnode *typ;
  int sto;
  Value val;
}
Node;
struct Scope
{
  Table *table;
} *sp;
static Node op (Node);
Entry *p, *q;
union YYSTYPE
{
  Symbol *sym;
  Node rec;
};
typedef union YYSTYPE YYSTYPE;
typedef short int yytype_int16;

int
yyparse (void)
{
  YYSTYPE yyval;
  int yyn;
  YYSTYPE *yyvsp;
  while (1)
    {
      if (yyn == 34)
	{
	  if ((yyvsp[-1].rec).sto & 0x10)
	    sprintf (errbuf, "invalid typedef qualifier for '%s'",
		     (yyvsp[0].sym)->name);
	  p = enter (sp->table, (yyvsp[0].sym));
	}
      else
	op ((yyvsp[0].rec));
      *++yyvsp = yyval;
    }
}

static Node
op (Node q)
{
  integer (q.typ);
  mgtype (q.typ);
}


/* { dg-final { scan-rtl-dump-times "\\\(set \\\(reg:SI 5 \\\$5\\\)" 2 "expand" { target { ilp32 } } } }  */
/* { dg-final { scan-rtl-dump-times "\\\(set \\\(reg:SI 6 \\\$6\\\)" 1 "expand" { target { ilp32 } } } }  */

/* { dg-final { scan-rtl-dump-times "\\\(set \\\(reg:DI 5 \\\$5\\\)" 2 "expand" { target { lp64 } } } }  */
/* { dg-final { scan-rtl-dump-times "\\\(set \\\(reg:DI 6 \\\$6\\\)" 1 "expand" { target { lp64 } } } }  */
