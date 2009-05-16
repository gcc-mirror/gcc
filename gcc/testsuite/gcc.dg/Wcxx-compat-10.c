/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

struct s1 { int f; };
typedef int s2;
void
f1 ()
{
  typedef int s1;
  struct s2 { int f; };
}

struct s3 { int f; };
typedef struct s3 s3;

typedef struct s4 s4;
struct s4 { int f; };

struct s5 { int f; };	/* { dg-message "note: originally defined here" } */
typedef int s5;		/* { dg-warning "invalid in C\[+\]\[+\]" } */

typedef int s6;		/* { dg-message "note: originally defined here" } */
struct s6 { int f; };	/* { dg-warning "invalid in C\[+\]\[+\]" } */

void
f2 ()
{
  struct s7 { int f; };	/* { dg-message "note: originally defined here" } */
  typedef int s7;	/* { dg-warning "invalid in C\[+\]\[+\]" } */

  typedef int s8;	/* { dg-message "note: originally defined here" } */
  struct s8 { int f; };	/* { dg-warning "invalid in C\[+\]\[+\]" } */

  struct s9 { int f; };
  { typedef int s9; }

  typedef int s10;
  { struct s10 { int f; }; }
}

enum e1 { A };
typedef int e2;
void
f3 ()
{
  typedef int e1;
  enum e2 { B };
}

enum e3 { C };
typedef enum e3 e3;

enum e5 { E };		/* { dg-message "note: originally defined here" } */
typedef int e5;		/* { dg-warning "invalid in C\[+\]\[+\]" } */

typedef int e6;		/* { dg-message "note: originally defined here" } */
enum e6 { F };		/* { dg-warning "invalid in C\[+\]\[+\]" } */

void
f4 ()
{
  enum e7 { G };	/* { dg-message "note: originally defined here" } */
  typedef int e7;	/* { dg-warning "invalid in C\[+\]\[+\]" } */

  typedef int e8;	/* { dg-message "note: originally defined here" } */
  enum e8 { H };	/* { dg-warning "invalid in C\[+\]\[+\]" } */

  enum e9 { I };
  { typedef int e9; }

  typedef int e10;
  { enum e10 { J }; }
}
