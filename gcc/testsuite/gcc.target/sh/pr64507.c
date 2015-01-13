/* Check that the __builtin_strnlen returns 0 with with 
   non-constant 0 length.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern int snprintf(char *, int, const char *, ...);
extern void abort (void);

int main()
 {
   int i;
   int cmp = 0;
   char buffer[1024];
   const char* s = "the string";

   snprintf(buffer, 4, "%s", s);

   for (i = 1; i < 4; i++)
     cmp += __builtin_strncmp(buffer, s, i - 1);

  if (cmp)
    abort();

  return 0;
}
