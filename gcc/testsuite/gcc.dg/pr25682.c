/* PR c/25682 */
/* { dg-do compile } */
/* { dg-options "-ansi" } */
/* Test whether we don't ICE on questionable constructs where offsetof
   should have been used instead.  */

struct S
{
  char a[4];
  int b;
};

char c[(char *) &((struct S *) 0)->b - (char *) 0]; /* { dg-warning "variably modified" } */
char d[(__UINTPTR_TYPE__) &((struct S *) 8)->b]; /* { dg-warning "variably modified" } */
char e[sizeof (c) == __builtin_offsetof (struct S, b) ? 1 : -1];
char f[sizeof (d) == __builtin_offsetof (struct S, b) + 8 ? 1 : -1];

extern void bar (char *, char *);

void
foo (void)
{
  char g[(char *) &((struct S *) 0)->b - (char *) 0];
  char h[(__UINTPTR_TYPE__) &((struct S *) 8)->b];
  char i[sizeof (g) == __builtin_offsetof (struct S, b) ? 1 : -1];
  char j[sizeof (h) == __builtin_offsetof (struct S, b) + 8 ? 1 : -1];
  bar (g, h);
}
