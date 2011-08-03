/* { dg-do compile } */

typedef union rtunion_def {
  struct rtx_def *rtx;
} rtunion;
typedef struct rtx_def {
  unsigned short code;
  rtunion fld[1];
} *rtx;
extern rtx recog_operand[];
extern rtx *recog_operand_loc[];
extern int reload_n_operands;
extern void find_dummy_reload (int, int);
extern int asm_noperands (rtx);
extern int n_occurrences (char **);
char operands_match[10][10];
void find_reloads (rtx insn, int n_alternatives, int commutative)
{
  register int i, j;
  int noperands;
  char *constraints[10];
  int address_reloaded[10];
  int this_alternative[10];
  char this_alternative_win[10];
  int this_alternative_matches[10];
  int this_alternative_number;
  rtx body = ((insn)->fld[3].rtx);
  int operand_mode[10];
  if (body->code == 1)
    {
      reload_n_operands = noperands = asm_noperands (body);
      n_alternatives = n_occurrences (constraints);
    }
  for (this_alternative_number = 0;
       this_alternative_number < n_alternatives;
       this_alternative_number++)
    for (i = 0;
	 i < noperands;
	 i++)
      {
	register char *p = constraints[i];
	register int win = 0;
	int badop = 1;
	int c;
	register rtx operand = recog_operand[i];
	int force_reload = 0;
	this_alternative_win[i] = 0;
	this_alternative[i] = 1;
	while (*p && (c = *p++) != ',')
	  switch (c)
	    {
	    case '4':
	      c -= '0';
	      this_alternative_matches[i] = c;
	      if ((c != commutative
		   || i != commutative + 1)
		  && operands_match[c][i])
		win = this_alternative_win[c];
	      else
		find_dummy_reload (operand_mode[i], this_alternative[c]);
	      if (! win || force_reload)
		for (j = 0; j < i; j++)
		  if (this_alternative_matches[j]
		      == this_alternative_matches[i])
		    badop = 1;
	      break;
	    case '<':
	      if (operand->code == 2
		  && ! address_reloaded[i]
		  && operand->fld[0].rtx->code == 3)
		win = 1;
	    }
      }
}
