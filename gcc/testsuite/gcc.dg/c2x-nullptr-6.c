/* PR c/108424 */
/* { dg-options "-std=c2x" } */

struct S {
  int i;
  int : nullptr; /* { dg-error "not an integer constant" } */
};

enum E { X = nullptr }; /* { dg-error "not an integer constant" } */

alignas(nullptr) int g; /* { dg-error "not an integer constant" } */

int arr[10] = { [nullptr] = 1 }; /* { dg-error "not of integer type" } */

_Static_assert (nullptr, "nullptr"); /* { dg-error "not an integer" } */

void f (int n)
{
  switch (n) {
  case nullptr: /* { dg-error "an integer constant" } */
  default:
  }

  switch (n) {
  case 1 ... nullptr: /* { dg-error "an integer constant" } */
  default:
  }

  switch (n) {
  case nullptr ... 2: /* { dg-error "an integer constant" } */
  default:
  }
}
