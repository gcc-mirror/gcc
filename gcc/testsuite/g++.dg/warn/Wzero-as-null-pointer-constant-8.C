// PR c++/71302
// { dg-options "-Wzero-as-null-pointer-constant -fdiagnostics-show-caret" }

#include <cstddef>

static void
callee_1 (int param1, const char* param2, int param3) {}

void
test_1 (int param1, const char* param2, int param3)
{
  callee_1 (0, 0, 0); // { dg-warning "16: zero as null pointer constant" }
  /* { dg-begin-multiline-output "" }
   callee_1 (0, 0, 0);
                ^
     { dg-end-multiline-output "" } */

  callee_1 (0, NULL, 0);
}

template <typename T>
void
callee_2 (int param1, T* param2, int param3) {}

void
test_2 (int param1, const char* param2, int param3)
{
  callee_2<const char*> (0, 0, 0); // { dg-warning "29: zero as null pointer constant" }
  /* { dg-begin-multiline-output "" }
   callee_2<const char*> (0, 0, 0);
                             ^
     { dg-end-multiline-output "" } */

  callee_2<const char*> (0, NULL, 0);
}

void
test_3 ()
{
  const char *msg_a = 0; // { dg-warning "23: zero as null pointer constant" }
  /* { dg-begin-multiline-output "" }
   const char *msg_a = 0;
                       ^
     { dg-end-multiline-output "" } */

  const char *msg_b = NULL;
}
