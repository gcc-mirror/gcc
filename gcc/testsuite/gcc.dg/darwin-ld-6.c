/* Test Darwin linker option -nofixprebinding.  */
/* Developed by Devang Patel <dpatel@apple.com>.  */

/* { dg-options "-nofixprebinding" } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}

