/* The RTL loop optimizer used to replace the output register of the
   inline assembly with a pseudo although the variable is declared as
   register asm ("0").  */

/* { dg-do run } */
/* { dg-options "-O2 -Wno-attributes" } */

extern void abort (void);

static unsigned char __attribute__ ((always_inline))
mytoupper (unsigned char c)
{
  if (c >= 'a' && c <= 'z')
    c -= 'a' - 'A';
  return c;
}

static unsigned long __attribute__ ((always_inline))
strlen (const char *s)
{
  register unsigned long r0 asm ("0");
  const char *tmp = s;

  asm (
#ifdef __s390x__
       "  lghi  %0, 0\n"
#else
       "  lhi   %0, 0\n"
#endif
       "0:srst  %0,%1\n"
       "  jo    0b"
       : "=d" (r0), "+a" (tmp)
       :
       :"cc");
  return r0 - (unsigned long) s;
}

char boot_command_line[] = "this is a test";

void __attribute__ ((noinline))
foo (char *str)
{
  if (strcmp (str, "THIS IS A TEST") != 0)
    abort ();
}

int
main ()
{
  char upper_command_line[1024];
  int i;

  for (i = 0; i < strlen (boot_command_line); i++)
    upper_command_line[i] = mytoupper (boot_command_line[i]);

  upper_command_line[strlen (boot_command_line)] = 0;
  foo (upper_command_line);

  return 0;
}
