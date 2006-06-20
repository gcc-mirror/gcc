/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

struct funny_match
{
  int this, other;
};

typedef struct rtx {
    int code;
} *rtx;

extern rtx recog_operand[];
extern int which_alternative;
extern int nalternatives;

int
constrain_operands (insn_code_num, strict)
     int insn_code_num;
     int strict;
{
  char *constraints[10];
  struct funny_match funny_match[10];
  register int c;
  int funny_match_index;

  which_alternative = 0;

  while (which_alternative < nalternatives)
    {
      register int opno;
      register char *p = constraints[opno];
      int lose = 0;
      funny_match_index = 0;

      while (*p && (c = *p++) != ',')
	funny_match[funny_match_index++].other = c - '0';

      if ((((recog_operand[opno])->code) == 12))
	lose = 1;

      if (!lose)
	{
	  while (--funny_match_index >= 0)
	    recog_operand[funny_match[funny_match_index].other]
		    = recog_operand[funny_match[funny_match_index].this];
	  return 1;
	}
      which_alternative++;
    }

  if (strict == 0)
    return constrain_operands (insn_code_num, -1);
  return 0;
}

