/* Test Darwin linker option -bind_at_load.  */
/* Developed by Devang Patel <dpatel@apple.com>.  */

/* { dg-options "-bind_at_load" } */
/* { dg-do link { target powerpc-*-darwin* } } */

int main()
{
  return 0;
}

