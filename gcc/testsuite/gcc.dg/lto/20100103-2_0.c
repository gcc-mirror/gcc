/* { dg-lto-do link } */
/* { dg-lto-options {{-O -flto -funsigned-char} {-O -flto -fsigned-char}} } */

char p[32] = "";
int main ()
{
  if (__builtin___strcpy_chk (p + 1, "vwxyz",
			      __builtin_object_size (p + 1, 0)) != p + 1)
    __builtin_abort ();
  return 0;
} 

