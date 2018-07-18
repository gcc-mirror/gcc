/* { dg-do compile } */

typedef int jmp_buf[1];

extern void exit(int) __attribute__((__noreturn__));
extern int setjmpx(jmp_buf) __attribute__((__returns_twice__));

jmp_buf jbAnagram;
int a[6];
int d;
int b () { exit (1); }
int c () { b (); }
int e ()
{
  int f = 0;
  for (; f < 6; f++)
    a[f] = d;
  c ();
  setjmpx (jbAnagram);
}
