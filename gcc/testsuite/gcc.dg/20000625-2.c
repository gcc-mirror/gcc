/* More paste corner cases from glibc.  */
/* { dg-do run } */

#define symbol_version(name, version) name##@##version

#define str(x) xstr(x)
#define xstr(x) #x

const char a[] = str(symbol_version(getrlimit, GLIBC_2.0));
const char b[] = str(getrlimit@GLIBC_2.0);
const char c[] = "getrlimit@GLIBC_2.0";

#include <stdlib.h>
#include <string.h>

int
main(void)
{
  if(strcmp(a, b))
    abort();
  if(strcmp(b, c))
    abort();
  if(strcmp(c, a))
    abort();

  return 0;
}
