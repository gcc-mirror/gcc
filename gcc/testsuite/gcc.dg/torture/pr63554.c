/* PR c/63554 - ice in "execute_todo, at passes.c:1797" with -O3
   { dg-do compile } */

char *a;
void
nssutil_ReadSecmodDB (void)
{
  long b = __builtin_object_size (0, 0);
  a = __builtin___strncat_chk (a, " ", 1, b);
}

/* { dg-prune-output "\\\[-Wstringop-overflow=]" } */
