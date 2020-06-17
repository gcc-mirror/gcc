/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */


char one[50];
char two[50];

void
test_strncat (void)
{
  (void) __builtin_strcpy (one, "gh");
  (void) __builtin_strcpy (two, "ef");
 
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow="
#pragma GCC diagnostic ignored "-Warray-bounds"
  (void) __builtin_strncat (one, two, 99); 
#pragma GCC diagnostic pop
}

