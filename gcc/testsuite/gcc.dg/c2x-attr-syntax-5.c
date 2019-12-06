/* Test C2x attribute syntax.  Test unknown standard attributes
   diagnosed with a pedwarn.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[unknown_attribute]]; /* { dg-error "attribute ignored" } */

[[unknown_attribute]] extern int a; /* { dg-error "attribute ignored" } */
extern int [[unknown_attribute(123)]] a; /* { dg-error "attribute ignored" } */
extern int a [[unknown_attribute("")]]; /* { dg-error "attribute ignored" } */

int f () [[unknown_attribute]]; /* { dg-error "attribute ignored" } */
int f (void) [[unknown_attribute(1)]]; /* { dg-error "attribute ignored" } */
int g ([[unknown_attribute]] int a); /* { dg-error "attribute ignored" } */
int g (int [[unknown_attribute]] a); /* { dg-error "attribute ignored" } */
int g (int a [[unknown_attribute]]); /* { dg-error "attribute ignored" } */
int g ([[unknown_attribute]] int); /* { dg-error "attribute ignored" } */
int g (int [[unknown_attribute]]); /* { dg-error "attribute ignored" } */
int g (int) [[unknown_attribute]]; /* { dg-error "attribute ignored" } */

int *[[unknown_attribute]] p; /* { dg-error "attribute ignored" } */
int b[3] [[unknown_attribute]]; /* { dg-error "attribute ignored" } */

int h (int () [[unknown_attribute]]); /* { dg-error "attribute ignored" } */

struct [[unknown_attribute]] s; /* { dg-error "attribute ignored" } */
union [[unknown_attribute]] u; /* { dg-error "attribute ignored" } */

struct [[unknown_attribute]] s2 { int a; }; /* { dg-error "attribute ignored" } */
union [[unknown_attribute(x)]] u2 { int a; }; /* { dg-error "attribute ignored" } */

struct s3 { [[unknown_attribute]] int a; }; /* { dg-error "attribute ignored" } */
struct s4 { int [[unknown_attribute]] a; }; /* { dg-error "attribute ignored" } */
union u3 { [[unknown_attribute]] int a; }; /* { dg-error "attribute ignored" } */
union u4 { int [[unknown_attribute]] a; }; /* { dg-error "attribute ignored" } */

int z = sizeof (int [[unknown_attribute]]); /* { dg-error "attribute ignored" } */

enum [[unknown_attribute]] { E1 }; /* { dg-error "attribute ignored" } */
enum { E2 [[unknown_attribute]] }; /* { dg-error "attribute ignored" } */
enum { E3 [[unknown_attribute]] = 4 }; /* { dg-error "attribute ignored" } */

void
func (void) [[unknown_attribute]] { /* { dg-error "attribute ignored" } */
  [[unknown_attribute]] int var; /* { dg-error "attribute ignored" } */
  [[unknown_attribute]] { } /* { dg-error "attribute ignored" } */
  [[unknown_attribute(!)]]; /* { dg-error "attribute ignored" } */
  [[unknown_attribute]] var = 1; /* { dg-error "attribute ignored" } */
  [[unknown_attribute]] x: var = 2; /* { dg-error "attribute ignored" } */
  for ([[unknown_attribute]] int zz = 1; zz < 10; zz++) ; /* { dg-error "attribute ignored" } */
}

/* nodiscard is not yet implemented, but is a standard attribute, so
   its use is not a constraint violation and should only receive a
   warning.  */
[[nodiscard]] int ndfunc (void); /* { dg-warning "attribute directive ignored" } */
