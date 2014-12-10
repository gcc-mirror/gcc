/* { dg-do compile } */

char *a;
void
nssutil_ReadSecmodDB (void)
{
  long b = __builtin_object_size (0, 0);
  a = __builtin___strncat_chk (a, " ", 1, b);
}
