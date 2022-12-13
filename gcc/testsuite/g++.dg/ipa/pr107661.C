/* { dg-do run  { target c++11 } } */
/* { dg-options "-O1 -fipa-cp -fipa-cp-clone" } */

struct R {} RGood;
struct L {} LBad;

volatile int vi;
static void __attribute__((noipa)) L_run(void) { vi = 0; __builtin_abort (); }
static void callback_fn_L(void) { vi = 1; L_run(); }
static void callback_fn_R(void) { vi = 2; }

struct function_ref {
  void (*callback)(void) = nullptr;

  function_ref(L * pl) { callback = callback_fn_L; }
  function_ref(R * pr) { callback = callback_fn_R; }
};

// allow one level of recursion to call callback twice
static int is_recur(void) {
    static int n = 0;
    switch (n++) {
      case 0: return 1;
      default: return 0;
    }
}

static void do3(volatile int * punused, function_ref Expired) {
  Expired.callback();

  if (is_recur())
      do3(punused, Expired);
}

static void do1(function_ref Expired) {
  volatile int unused = 42;

  do3(&unused, Expired);
}

int main(int, const char **) { do1(&RGood); return 0; }

void seemingly_unused_foo(void) { do1(&LBad); }

void (*fnptr)(void) = seemingly_unused_foo;
