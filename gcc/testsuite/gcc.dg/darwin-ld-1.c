/* Test Darwin linker option -all_load.  */
/* Developed by Devang Patel <dpatel@apple.com>.  */

/* { dg-options "-all_load" } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}

