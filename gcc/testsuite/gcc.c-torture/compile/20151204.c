/* { dg-skip-if "Array too big" { "avr-*-*" "pdp11-*-*" } } */

typedef __SIZE_TYPE__ size_t;

int strcmp (const char*, const char*);
void *memchr (const void *, int, size_t);
char* strncpy (char *, const char *, size_t);

int
main (int argc, char** argv)
{
  char target[32753] = "A";
  char buffer[32753];
  char *x;
  x = buffer;

  if (strcmp (target, "A")
      || memchr (target, 'A', 0) != ((void *) 0))
    if (strncpy (x, "", 4) != x);
  return 0;
}
