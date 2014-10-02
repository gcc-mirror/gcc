/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

struct rtx_def
{
  int a;
};

typedef struct rtx_def *rtx;

struct rd {
  int alternative_enabled_p[100];
  rtx operand[100];
  int n_operands;
};

rtx this_insn;
int n_reloads;
int n_replacements;
int n_earlyclobbers;
int replace_reloads;
int hard_regs_live_known;
short* static_reload_reg_p;
struct rd recog_data;
int ix86_preferred_reload_class (rtx, int);

int
find_reloads (rtx insn, int replace, int ind_levels, int live_known,
	      short *reload_reg_p)
{
  int i, j;
  int noperands = replace;

  int no_input_reloads = 0;
  int n_alternatives = replace;
  char this_alternative_match_win[30];
  char this_alternative_win[30];
  char this_alternative_earlyclobber[30];
  int this_alternative_matches[30];
  int goal_alternative[30];
  int this_alternative_number;

  char goal_alternative_match_win[30];
  char goal_alternative_win[30];
  int best;

  int operand_mode[30];
  int retval = 0;

  for (this_alternative_number = 0;
       this_alternative_number < n_alternatives;
       this_alternative_number++)
    {

      int losers = 0;
      int bad = 0;
      
      if (!recog_data.alternative_enabled_p[this_alternative_number])
	{
	  int i;
	  
	  for (i = 0; i < recog_data.n_operands; i++)
	    ;
	  
	  continue;
	}
      
      for (i = 0; i < noperands; i++)
	if (this_alternative_earlyclobber[i]
	    && (this_alternative_win[i] || this_alternative_match_win[i]))
	  {
	    if (j != noperands)
	      {
		losers++;
		
		for (j = 0; j < noperands; j++)
		  if (this_alternative_matches[j] == i
		      && this_alternative_match_win[j])
		    {
		      this_alternative_win[j] = 0;
		      this_alternative_match_win[j] = 0;
		      losers++;
		    }
	      }
	  }
      
      if (losers == 0)
	{
	  for (i = 0; i < noperands; i++)
	    {
	      goal_alternative_win[i] = 0;
	      goal_alternative_match_win[i] = 0;
	    }
	  
	  goto finish;
	}

      if (! bad && best > losers)
	{
	  for (i = 0; i < noperands; i++)
	    {
	      goal_alternative[i] = 0;
	      goal_alternative_win[i] = 0;
	    }
	}
    }
  

 finish:

  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i])
      {
	rtx op = recog_data.operand[i];
	int mode = operand_mode[i];
	
	if (((ix86_preferred_reload_class ((op), (goal_alternative[i])) == 2)
	     || no_input_reloads)
	    && mode != 0)
	  {}
      }

  return retval;
}

