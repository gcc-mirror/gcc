/* { dg-options "-fgnu89-inline" } */

#include <stdbool.h>
#include <stdlib.h>
extern void abort (void);
int strcmp (const char*, const char*);
char s[2048] = "a";
inline bool foo(const char *str) {
  return !strcmp(s,str);
}
int main() {
int i = 0;
  while(!(foo(""))) {
    i ++;
    s[0] = '\0';
    if (i>2)
     abort ();
  }
  return 0;
}

