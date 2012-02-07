/* PR middle-end/51994 */
/* Testcase by Uros Bizjak <ubizjak@gmail.com> */

extern char *strcpy (char *, const char *);
extern void abort (void);

char __attribute__((noinline))
test (int a)
{
  char buf[16];
  char *output = buf;

  strcpy (&buf[0], "0123456789");

  output += a;
  output -= 1;

  return output[0];
}

int main ()
{
  if (test (2) != '1')
    abort ();

  return 0;
}
