/* { dg-do compile { target ia64-*-linux* } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "@ltoffx\\(os_boot_rendez#\\)" } } */
/* { dg-final { scan-assembler "@ltoff\\(@fptr\\(os_boot_rendez#\\)\\)" } } */

/* Test function descriptor access.  */

struct ia64_fdesc
{
  unsigned long func;
  unsigned long gp;
};

void
os_boot_rendez (void)
{
}

extern int check (unsigned long);

int
main (int argc, char **argv)
{
  int i;
  int res = 0;

  for (i = 0; i < 1; i++)
    res += check (((struct ia64_fdesc *) os_boot_rendez)->gp);
  return res;
}
