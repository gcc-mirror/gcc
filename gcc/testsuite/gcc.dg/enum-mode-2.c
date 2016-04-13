/* PR c/51147 - attribute((mode(byte))) on an enum generates wrong code */

/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-tree-optimized" } */

enum _eq_bool
{
  false,
  true
} __attribute__((mode (byte)));

typedef enum _eq_bool bool;

bool foo (void);
bool bar (void);

bool test (void)
{
  return foo () || bar ();
}

/* { dg-final { scan-tree-dump-times "foo|bar" 2 "optimized" } } */
