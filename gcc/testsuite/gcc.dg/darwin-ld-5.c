/* Test Darwin linker option -bundle_loader.  */
/* Developed by Devang Patel <dpatel@apple.com>.  */

/* { dg-options "-bundle -bundle_loader foo" } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}

