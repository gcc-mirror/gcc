// PR c/25682
// { dg-do compile }
// Test whether we don't ICE on questionable constructs where offsetof
// should have been used instead.

struct S
{
  char a[4];
  int b;
};

extern void bar (char *, char *);

void
foo (void)
{
  char g[(char *) &((struct S *) 0)->b - (char *) 0]; // { dg-error "40:size of array .g. is not an integral constant-expression" }
  char h[(__SIZE_TYPE__) &((struct S *) 8)->b];	      // { dg-error "10:size of array .h. is not an integral constant-expression" }
  bar (g, h);
}
