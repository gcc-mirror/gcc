/* PR c/34457 */
/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "--combine -O2" } */
/* { dg-additional-sources "pr34457-2.c" } */
   

typedef __SIZE_TYPE__ size_t;
extern int printf (const char *, ...);
extern void *memset (void *, int, size_t);

int bar (int (*)(), int, void *);

int
main(int argc, char **argv)
{
  struct s { int a; char b[argc]; };
  int nested (struct s x) { return x.a + sizeof(x); }
  struct s t;
  memset (&t, 0, sizeof(t));
  t.a = 123;
  printf("%d\n", bar (nested, argc, &t));
  return 0;
}
