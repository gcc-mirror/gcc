/* { dg-do run } */
/* { dg-require-effective-target arm_eabi } */

#include <stdarg.h>
#include <stddef.h>

/* AAPCS \S 7.1.4 requires that va_list match the structure shown
   here */
typedef struct my_va_list 
{
  void *ap;
} my_va_list;

int 
main () {
  if (sizeof (va_list) != sizeof (my_va_list))
    return 1;
  /* This check confirms both that "va_list" has a member named "__ap"
     and that it is located at the correct position.  */
  if (offsetof (va_list, __ap) 
      != offsetof (my_va_list, ap))
    return 2;

  return 0;
}
