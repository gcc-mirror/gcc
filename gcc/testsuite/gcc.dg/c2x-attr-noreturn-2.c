/* Test C2x noreturn attribute: invalid contexts.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[noreturn]]; /* { dg-error "ignored" } */

int [[noreturn]] var; /* { dg-error "ignored" } */

int array_with_dep_type[2] [[noreturn]]; /* { dg-error "ignored" } */

void fn_with_dep_type () [[noreturn]]; /* { dg-error "ignored" } */

int z = sizeof (int [[__noreturn__]]); /* { dg-error "ignored" } */

[[noreturn]] int x1; /* { dg-error "can only be applied to functions" } */
int x2 [[__noreturn__]]; /* { dg-error "can only be applied to functions" } */

[[_Noreturn]] void (*fp) (); /* { dg-error "can only be applied to functions" } */

void
f (void)
{
  int a;
  [[_Noreturn]]; /* { dg-error "ignored" } */
  [[___Noreturn__]] a = 1; /* { dg-error "ignored" } */
}

int
g ([[noreturn]] int x, int y) /* { dg-error "can only be applied to functions" } */
{
  [[noreturn]] typedef float F; /* { dg-error "can only be applied to functions" } */
  [[noreturn]] int a; /* { dg-error "can only be applied to functions" } */
  int b [[__noreturn__]]; /* { dg-error "can only be applied to functions" } */
  int c [[noreturn]]; /* { dg-error "can only be applied to functions" } */
  [[__noreturn__]] label1: ; /* { dg-error "can only be applied to functions" } */
  c = y;
  [[noreturn]] label2: ; /* { dg-error "can only be applied to functions" } */
  return y;
}

struct [[_Noreturn]] s { double d; }; /* { dg-error "can only be applied to functions" } */

struct s2
{
  [[___Noreturn__]] int a; /* { dg-error "can only be applied to functions" } */
  int b [[noreturn]]; /* { dg-error "can only be applied to functions" } */
} x;

enum e { E1 [[noreturn]] }; /* { dg-error "can only be applied to functions" } */

union [[_Noreturn]] u { int x; }; /* { dg-error "can only be applied to functions" } */

enum [[noreturn]] eu { E2 }; /* { dg-error "can only be applied to functions" } */

void fx ([[noreturn]] int p); /* { dg-error "can only be applied" } */

union u2
{
  [[noreturn]] int a; /* { dg-error "can only be applied to functions" } */
  int b [[noreturn]]; /* { dg-error "can only be applied to functions" } */
} y;

void
g2 (int x)
{
  switch (x)
    {
      [[noreturn]] case 1: ; /* { dg-error "can only be applied to functions" } */
      [[__noreturn__]] case 2: ; /* { dg-error "can only be applied to functions" } */
      [[noreturn]] default: ; /* { dg-error "can only be applied to functions" } */
    }
}
