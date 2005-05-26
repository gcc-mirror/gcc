/* PR optimization/9888 */
/* Originator: Jim Bray <jb@as220.org> */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mtune=k6 -Os" } */

enum reload_type
{
  RELOAD_FOR_INPUT, RELOAD_FOR_OUTPUT, RELOAD_FOR_INSN,
  RELOAD_FOR_INPUT_ADDRESS, RELOAD_FOR_INPADDR_ADDRESS,
  RELOAD_FOR_OUTPUT_ADDRESS, RELOAD_FOR_OUTADDR_ADDRESS,
  RELOAD_FOR_OPERAND_ADDRESS, RELOAD_FOR_OPADDR_ADDR,
  RELOAD_OTHER, RELOAD_FOR_OTHER_ADDRESS
};

#define FOO_SIZE 3

/* My results, varying with FOO_SIZE:
   30: asm error "value of ..fff77 too large:
   3 to 29: ....ff7d...
   1 to 2: no error.  */

struct reload
{
   int foo[FOO_SIZE];
   int opnum; 
   enum reload_type when_needed;
   unsigned int optional:1; 
   unsigned int secondary_p:1;
};

#define N_RELOADS  2

struct reload rld[N_RELOADS];
int n_reloads = N_RELOADS;

int main(void)
{
  int i;

  enum reload_type operand_type[1];

  enum reload_type address_type[1];

  int operand_reloadnum[1];
  int goal_alternative_matches[1];

  for (i = 0; i < n_reloads; i++)
    {
      if (rld[i].secondary_p
          && rld[i].when_needed == operand_type[rld[i].opnum])
        rld[i].when_needed = address_type[rld[i].opnum];

      if ((rld[i].when_needed == RELOAD_FOR_INPUT_ADDRESS
           || rld[i].when_needed == RELOAD_FOR_OUTPUT_ADDRESS
           || rld[i].when_needed == RELOAD_FOR_INPADDR_ADDRESS
           || rld[i].when_needed == RELOAD_FOR_OUTADDR_ADDRESS)
          && (operand_reloadnum[rld[i].opnum] < 0
              || rld[operand_reloadnum[rld[i].opnum]].optional))
        {

          if (rld[i].when_needed == RELOAD_FOR_INPADDR_ADDRESS
              || rld[i].when_needed == RELOAD_FOR_OUTADDR_ADDRESS)
            rld[i].when_needed = RELOAD_FOR_OPADDR_ADDR;
          else
            rld[i].when_needed = RELOAD_FOR_OPERAND_ADDRESS;
        }

      if ((rld[i].when_needed == RELOAD_FOR_INPUT_ADDRESS
           || rld[i].when_needed == RELOAD_FOR_INPADDR_ADDRESS)
          && operand_reloadnum[rld[i].opnum] >= 0
          && (rld[operand_reloadnum[rld[i].opnum]].when_needed
              == RELOAD_OTHER))
        rld[i].when_needed = RELOAD_FOR_OTHER_ADDRESS;

      if (goal_alternative_matches[rld[i].opnum] >= 0)
        rld[i].opnum = goal_alternative_matches[rld[i].opnum];
    }

  return 0;
}
