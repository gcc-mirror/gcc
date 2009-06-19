/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
const int v1;			/* { dg-warning "invalid in C\[+\]\[+\]" } */
const char * const v2;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
struct s { int f1; int f2; };
const struct s v3;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
const int v4 = 1;
const char * const v5 = 0;
const struct s v6 = { 0, 0 };
const struct s v7 = { 0 };
void
f()
{
  const int v11;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  const char * const v12;	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  const struct s v13;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  const int v14 = 1;
  const char * const v15 = 0;
  const struct s v16 = { 0, 0 };
  const struct s v17 = { 0 };
}
