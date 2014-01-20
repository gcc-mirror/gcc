/* { dg-do compile } */
/* { dg-options "-O" } */

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__gnu_inline__)) __attribute__ ((__artificial__)) char * __attribute__ ((__nothrow__ , __leaf__))
strcat (char *__restrict __dest, const char *__restrict __src)
{
  return __builtin___strcat_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}
static char raw_decode;
void foo (char **argv, char *outfilename)
{
  if (**argv == 'r')
    raw_decode = 1;
  strcat (outfilename, raw_decode ? ".raw" : ".wav");
}
