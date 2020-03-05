/* PR middle-end/93399 */
/* { dg-do assemble } */
/* { dg-options "-fverbose-asm -dA -g -O3" } */

extern inline __attribute__ ((__always_inline__, __gnu_inline__)) char *
strstr (const char *haystack, const char *needle)
{
  return __builtin_strstr (haystack, needle);
}

int
main (int argc, const char **argv)
{
  char *substr = strstr (argv[0], "\n");
  char *another = strstr (argv[0], "\r\n");
  return 0;
}
