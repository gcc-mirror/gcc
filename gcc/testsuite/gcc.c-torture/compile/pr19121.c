/* { dg-additional-options "-fpermissive" } */
typedef struct interpreter {
  char Itokenbuf[256];
} PerlInterpreter;
static inline void S_missingterm(char *s)
{
  char tmpbuf[3] = "";
  char q;
  if (!s)
    s = tmpbuf;
  q = strchr(s,'"') ? '\'' : '"';
}
void S_scan_heredoc(PerlInterpreter *my_perl, char *s, int i)
{
  char term;
  term = *my_perl->Itokenbuf;
  if (i)
  {
    *s = term;
    S_missingterm(my_perl->Itokenbuf);
  }
  else
    S_missingterm(my_perl->Itokenbuf);
}
