/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
/* On Alpha EV4, dead code elimination and cfg simplification conspired
   to leave the register containing 'C' marked live, though all references
   to the variable had been removed.  */

struct operand_data
{
  struct operand_data *next;
  int index;
  const char *predicate;
  const char *constraint;
  int mode;
  unsigned char n_alternatives;
  char address_p;
  char strict_low;
  char eliminable;
  char seen;
};

struct data
{
  struct data *next;
  const char *name;
  const char *template;
  int code_number;
  int index_number;
  int lineno;
  int n_operands;
  int n_dups;
  int n_alternatives;
  int operand_number;
  int output_format;
  struct operand_data operand[40];
};

extern void message_with_line (int, const char *, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));
extern int have_error;

extern char *strchr (__const char *__s, int __c) __attribute__ ((__pure__));

void
validate_insn_alternatives (d)
     struct data *d;
{
  int n = 0, start;

  for (start = 0; start < d->n_operands; start++)
    if (d->operand[start].n_alternatives > 0)
      {
        int len, i;
        const char *p;
        char c;	 /* { dg-bogus "used uninitialized" "uninitialized variable warning" } */
        int which_alternative = 0;
        int alternative_count_unsure = 0;

        for (p = d->operand[start].constraint; (c = *p); p += len)
          {
            len = 1;

            if (len < 1 || (len > 1 && strchr (",#*+=&%!0123456789", c)))
              {
                message_with_line (d->lineno,
                                   "invalid length %d for char '%c' in alternative %d of operand %d",
                                    len, c, which_alternative, start);
                len = 1;
                have_error = 1;
              }

            if (c == ',')
              {
                which_alternative++;
                continue;
              }

            for (i = 1; i < len; i++)
              if (p[i] == '\0')
                {
                  message_with_line (d->lineno,
                                     "NUL in alternative %d of operand %d",
                                     which_alternative, start);
                  alternative_count_unsure = 1;
                  break;
                }
              else if (strchr (",#*", p[i]))
                {
                  message_with_line (d->lineno,
                                     "'%c' in alternative %d of operand %d",
                                     p[i], which_alternative, start);
                  alternative_count_unsure = 1;
                }
          }
        if (alternative_count_unsure)
          have_error = 1;
        else if (n == 0)
          n = d->operand[start].n_alternatives;
        else if (n != d->operand[start].n_alternatives)
          {
            message_with_line (d->lineno,
                               "wrong number of alternatives in operand %d",
                               start);
            have_error = 1;
          }
      }


  d->n_alternatives = n;
}
