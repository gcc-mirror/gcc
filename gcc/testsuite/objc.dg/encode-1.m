/* Test if the Objective-C @encode machinery distinguishes between
   'BOOL *' (which should be encoded as a pointer to BOOL) and 'char *' (which
   should be encoded as '*').  This is somewhat tricky wrt the NeXT runtime,
   where we have 'typedef char BOOL'.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */
/* { dg-options "-fnext-runtime" } */
#include <string.h>           
#include <stdlib.h>
#include <objc/objc.h>

int main(void) {
  const char *BOOL_ptr = @encode(BOOL *);
  const char *BOOL_ = @encode(BOOL);
  const char *char_ptr = @encode(char *);

  if(*BOOL_ptr != '^' || strcmp(BOOL_ptr + 1, BOOL_))
    abort();

  if(strcmp(char_ptr, "*"))
    abort();

  return 0;
}
