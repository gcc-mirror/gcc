
/* Test Darwin linker option -arch_errors_fatal.  */
/* Developed by Devang Patel <dpatel@apple.com>.  */

/* { dg-options "-arch_errors_fatal" } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}

