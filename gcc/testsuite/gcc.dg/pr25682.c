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

char c[(char *) &((struct S *) 0)->b - (char *) 0]; /* { dg-error "variable-size" } */
char d[(__SIZE_TYPE__) &((struct S *) 8)->b]; /* { dg-error "variable-size" } */
char e[sizeof (c) == __builtin_offsetof (struct S, b) ? 1 : -1]; /* { dg-error "variably modified" } */
char f[sizeof (d) == __builtin_offsetof (struct S, b) + 8 ? 1 : -1]; /* { dg-error "variably modified" } */

extern void bar (char *, char *);

void
foo (void)
{
  char g[(char *) &((struct S *) 0)->b - (char *) 0];
  char h[(__SIZE_TYPE__) &((struct S *) 8)->b];
  char i[sizeof (g) == __builtin_offsetof (struct S, b) ? 1 : -1];
  char j[sizeof (h) == __builtin_offsetof (struct S, b) + 8 ? 1 : -1];
  bar (g, h);
}
