/* { dg-do compile } */
/* { dg-options "-march=armv7-a -mthumb -Os" }  */
/* { dg-final { scan-assembler "push\t\{r3" } } */
/* { dg-final { scan-assembler-not "r8" } } */

extern int hist_verify;
extern char *pre_process_line (char*);
extern char* savestring1 (char*, char*);
extern char* str_cpy (char*, char*);
extern int str_len (char*);
extern char* x_malloc (int);
#define savestring(x) (char *)str_cpy (x_malloc (1 + str_len (x)), (x))

char *
history_expand_line_internal (char* line)
{
  char *new_line;
  int old_verify;

  old_verify = hist_verify;
  hist_verify = 0;
  new_line = pre_process_line (line);
  hist_verify = old_verify;
  /* Two tail calls here, but r3 is not used to pass values.  */
  return (new_line == line) ? savestring (line) : savestring1 (new_line, line);
}
