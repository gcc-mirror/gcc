/* PR target/38902 */
/* { dg-do run } */
/* { dg-options "-O2 -fstack-protector" } */
/* { dg-require-effective-target fstack_protector } */

#ifdef DEBUG
#include <stdio.h>
#define debug(format, args...) printf (format , ## args)
#else
extern int sprintf (char *, const char *, ...);
#define debug(format, args...)
#endif

extern void abort (void);

/*

Copyright (C) 2009 Canonical, Ltd.
Author: Kees Cook <kees@ubuntu.com>
License: GPLv3

http://gcc.gnu.org/bugzilla/show_bug.cgi?id=38616
https://bugs.launchpad.net/ubuntu/+source/gcc-4.3/+bug/316019
http://gcc.gnu.org/bugzilla/show_bug.cgi?id=38902

gcc -O2 -fstack-protector truncate.c -o truncate

    Broken:

        Only the first operation fails, so create a new function for each test.
        Source must be local (literal or stack)

        __builtin_memmove
        __builtin_memcpy
        __builtin_strcpy  (optimized to __builtin_memcpy?)
        sprintf (direct)  (optmized to __builtin_strcpy?)
        sprintf (via %s)  (optmized to __builtin_strcpy?)

    OK:
        __builtin_strcat
        sprintf (complex format)

 */

char *heap = "1234567890abcdefghijklmnopqrstuvwxyz";

int failed = 0;

#define CHECK(count, a...) \
void test##count (void) \
{ \
  char *local = "1234567890abcdefghijklmnopqrstuvwxyz"; \
  char buffer[1024]=""; \
    a; \
    if (__builtin_strcmp(buffer, heap) == 0) { \
        debug("Okay(%d):\n\t%s\n", count, # a); \
    } \
    else { \
        debug("Failed(%d):\n\t%s\n", count, # a); \
	failed++; \
    } \
}


CHECK( 0, __builtin_memcpy (buffer, "1234567890abcdefghijklmnopqrstuvwxyz", __builtin_strlen("1234567890abcdefghijklmnopqrstuvwxyz")+1);                                        );
CHECK( 1, __builtin_memcpy (buffer, local, __builtin_strlen(local)+1);                );
CHECK( 2, __builtin_memcpy (buffer, heap, __builtin_strlen(heap)+1);                );

CHECK( 3, __builtin_memmove (buffer, "1234567890abcdefghijklmnopqrstuvwxyz", __builtin_strlen("1234567890abcdefghijklmnopqrstuvwxyz")+1);                                       );
CHECK( 4, __builtin_memmove (buffer, local, __builtin_strlen(local)+1);               );
CHECK( 5, __builtin_memmove (buffer, heap, __builtin_strlen(heap)+1);               );

CHECK( 6, __builtin_strcpy (buffer, "1234567890abcdefghijklmnopqrstuvwxyz");          );
CHECK( 7, __builtin_strcpy (buffer, local);                                      );
CHECK( 8, __builtin_strcpy (buffer, heap);                                      );

CHECK( 9,  sprintf (buffer, "1234567890abcdefghijklmnopqrstuvwxyz");         );
CHECK(10,  sprintf (buffer, local);                                     );
CHECK(11,  sprintf (buffer, heap);                                     );

CHECK(12,  sprintf (buffer, "%s", "1234567890abcdefghijklmnopqrstuvwxyz");   );
CHECK(13,  sprintf (buffer, "%s", local);                               );
CHECK(14,  sprintf (buffer, "%s", heap);                               );

CHECK(15, __builtin_strcat (buffer, "1234567890abcdefghijklmnopqrstuvwxyz");          );
CHECK(16, __builtin_strcat (buffer, local);                                      );
CHECK(17, __builtin_strcat (buffer, heap);                                       );

void mongoose(void)
{
  char buffer[1024]="";
  sprintf (buffer, "%s", "1234567890abcdefghijklmnopqrstuvwxyz");;
    if (__builtin_strcmp(buffer, heap) == 0) {
        debug("Okay(%d):\n\t%s\n", -1, "sprintf (buffer, \"%s\", \"1234567890abcdefghijklmnopqrstuvwxyz\");");
    }
    else {
        debug("Failed(%d):\n\t%s\n", -1, "sprintf (buffer, \"%s\", \"1234567890abcdefghijklmnopqrstuvwxyz\");");
	failed++;
    }
}

int main (int argc, char *argv[])
{
  test0();
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();
  test11();

  // wtf, why are these different?!
  test12();
  mongoose();

  test13();
  test14();
  test15();
  test16();
  test17();

  if (failed)
    abort ();

  return 0;
}
