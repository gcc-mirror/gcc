/* PR rtl-optimization/127421 */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v8 -fno-var-tracking -fno-var-tracking-assignments -fno-asynchronous-unwind-tables --param=max-delay-slot-insn-search=8 -fcompare-debug" } */

typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

extern int printf (const char *, ...);
extern int putchar (int);
extern int strcmp (const char *, const char *);

static uint64_t checksum;
static uint32_t values[3] =
  { 4294967295U, 4294967295U, 4294967295U };

static void
put_string (const char *s)
{
  int i = 0;
  while (s[i])
    putchar (s[i++]);
}

static void
put_hex_digit (int x)
{
  switch (x)
    {
    case 0: putchar ('0'); break;
    case 1: putchar ('1'); break;
    case 2: putchar ('2'); break;
    case 3: putchar ('3'); break;
    case 4: putchar ('4'); break;
    case 5: putchar ('5'); break;
    case 6: putchar ('6'); break;
    case 7: putchar ('7'); break;
    case 8: putchar ('8'); break;
    case 9: putchar ('9'); break;
    case 10: putchar ('a'); break;
    case 11: putchar ('b'); break;
    case 12: putchar ('c'); break;
    case 13: putchar ('d'); break;
    case 14: putchar ('e'); break;
    case 15: putchar ('f'); break;
    }
}

static void
print_checksum (uint64_t x)
{
  int i;
  put_string ("checksum = ");
  for (i = 0; i < 16; i++)
    {
      put_hex_digit (x & 15);
      x >>= 4;
    }
  putchar ('\n');
}

int
main (int argc, char **argv)
{
  int i;
  int verbose = argc == 2 && strcmp (argv[1], "1") == 0;

  for (i = 0; i < 3; i++)
    {
      checksum += values[i];
      if (verbose)
	printf ("index = [%d]\n", i);
    }
  print_checksum (checksum ^ 0xffffffffU);
  return 0;
}
