#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static int a;

int main(void)
{
  char *src = NULL;
  char buf[128];

  /* "a" can't have been touched yet, and thus
     is implicitly zero.  */
  switch (a) {
  case 1:
    strcpy(buf, src); /* { dg-bogus "NULL" } */
    break;
  case 0:
    strcpy(buf, "hello");
  }
  printf("%s\n", buf);
}
