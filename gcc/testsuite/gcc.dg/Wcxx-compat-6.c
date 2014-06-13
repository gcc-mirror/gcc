/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
enum E1 { A, B, C };
enum E2 { D, E, F };

enum E1 v1a = A;
enum E1 v1b = D;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
enum E1 v1c = 0;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
enum E1 v1d = (enum E1) 0;
enum E1 v1e = (enum E2) 0;	/* { dg-warning "invalid in C\[+\]\[+\]" } */

enum E2 v2a;

enum E1 a1[] =
{
  A,
  D,				/* { dg-warning "invalid in C\[+\]\[+\]" } */
  0,				/* { dg-warning "invalid in C\[+\]\[+\]" } */
  (enum E1) 0,
  (enum E2) 0,			/* { dg-warning "invalid in C\[+\]\[+\]" } */
  A
};

struct s1
{
  enum E1 e1;
};

struct s1 a2[] =
{
  { A },
  { D },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
  { 0 },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
  { (enum E1) 0 },
  { (enum E2) 0 },		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  { A }
};

struct s1 a3[] =
{
  [ 5 ] = { .e1 = A },
  [ 4 ] = { .e1 = D },		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  [ 3 ] = { .e1 = 0 },		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  [ 2 ] = { .e1 = (enum E1) 0 },
  [ 1 ] = { .e1 = (enum E2) 0 }, /* { dg-warning "invalid in C\[+\]\[+\]" } */
  [ 0 ] = { .e1 = A }
};

struct s2
{
  enum E1 e1 : 3;
};

struct s2 a4[] =
{
  { A },
  { D },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
  { 0 },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
  { (enum E1) 0 },
  { (enum E2) 0 },		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  { A }
};

struct s2 a5[] =
{
  [ 5 ] = { .e1 = A },
  [ 4 ] = { .e1 = D },		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  [ 3 ] = { .e1 = 0 },		/* { dg-warning "invalid in C\[+\]\[+\]" } */
  [ 2 ] = { .e1 = (enum E1) 0 },
  [ 1 ] = { .e1 = (enum E2) 0 }, /* { dg-warning "invalid in C\[+\]\[+\]" } */
  [ 0 ] = { .e1 = A }
};

void
f(enum E1 e1, enum E2 e2, struct s1 vs1, struct s1 *vp1)
{
  enum E1 va1[] = {
    e1,
    e2,				/* { dg-warning "invalid in C\[+\]\[+\]" } */
    v1a,
    v2a,			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    vs1.e1,
    vp1->e1,
    e1 ? e1 : e1,
    (0, e1)
  };

  struct s1 va2[] = {
    { e1 },
    { e2 },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    { v1a },
    { v2a },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    { vs1.e1 },
    { vp1->e1 },
    { e1 ? e1 : e1 },
    { (0, e1) }
  };

  struct s2 va3[] = {
    { e1 },
    { e2 },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    { v1a },
    { v2a },			/* { dg-warning "invalid in C\[+\]\[+\]" } */
    { vs1.e1 },
    { vp1->e1 },
    { e1 ? e1 : e1 },
    { (0, e1) }
  };
}

/* Match all extra informative notes.  */
/* { dg-message "near initialization for" "expected" { target *-*-* } 0 } */
