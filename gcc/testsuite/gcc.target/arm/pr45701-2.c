/* { dg-require-effective-target arm_arch_v7a_thumb_ok } */
/* { dg-options "-Os" } */
/* { dg-add-options arm_arch_v7a_thumb } */
/* { dg-final { scan-assembler "push\t\{r3" } } */
/* { dg-final { scan-assembler-not "push\t\[^\n\]*r8" } } */

extern char *pre_process_line (char*);
extern char* x_strdup (const char*);
extern char* x_strdup2 (const char*, const char*);
char *
history_expand_line_internal (char* line)
{
  char *new_line;
  new_line = pre_process_line (line);
  asm ("nop": : :"r4","r5","r6");
  /* Two tail calls here, but r3 is not used to pass values.  */
  return (new_line == line) ? x_strdup (line) : x_strdup2 (new_line, line);
}
