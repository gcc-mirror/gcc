// PR c++/103310
// Test that only manifestly-constant-evaluated comparisons lock a symbol's
// weakness.

// { dg-do compile { target c++17 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

extern void weakfn1 (void);
extern void weakfn2 (void);

void call_weakfn (void)
{
  if (weakfn1)
    weakfn1 ();
  if constexpr (weakfn2)
    weakfn2 ();
}

extern void weakfn1 (void)  __attribute__((weak));
extern void weakfn2 (void)  __attribute__((weak)); // { dg-error "declared weak after being used" }
