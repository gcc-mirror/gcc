/* PR c/11370  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunreachable-code" } */

extern int printf (const char *, ...);
extern void exit (int);

int main(int argc, char *argv[])
{
  if (argc != 1)
    exit(1);

  {
    int ix;  /* { dg-bogus "will never be executed" } */
    ix = printf("hello\n");
    printf("%d\n", ix);
  }

  return 0;
}

