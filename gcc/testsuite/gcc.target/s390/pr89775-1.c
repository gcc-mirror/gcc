/* PR target/89775 */
/* { dg-do run } */
/* { dg-options "-O0 -fomit-frame-pointer" } */
/* { dg-additional-sources "pr89775-2.c" } */

register void *sp __asm ("15");

__attribute__((noipa)) int
foo (const char *a, const char *b)
{
  while (1)
    {
      char c = *a++;
      if (c != *b++) return 0;
      if (c == '\0') return 1;
    }
}
