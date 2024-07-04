/* { dg-do compile } */
/* { dg-additional-options "--param logical-op-non-short-circuit=0" } */

extern char **environ;
static char ***p_environ = &environ;
int
_setenv_r (const char *name, const char *value)
{
  register char *C;
  int offset;
  for (C = (*p_environ)[offset]; (*C = *name++) && *C != '='; ++C);
  for (*C++ = '='; (*C++ = *value++) != 0;);
  return 0;
}
