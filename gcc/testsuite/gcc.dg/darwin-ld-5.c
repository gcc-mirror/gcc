/* Test Darwin linker option -dynamic.  */
/* Developed by Devang Patel <dpatel@apple.com>.  */

/* { dg-options "-dynamic" } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}

