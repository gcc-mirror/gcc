/* PR middle-end/97552 - missing waning passing null to a VLA argument
   declared [static]
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((__VA_ARGS__))

void             fptr_array (int(*)[0]);

void             fstatic_array (int[static 0]);
void A (nonnull) fnonnull_static_array (int [static 0]);

void             fvla (int n, int [n]);
void A (nonnull) fnonnull_vla (int n, int [n]);

void             fstatic_vla (int n, int [static n]);
void A (nonnull) fnonnull_static_vla (int n, int [static n]);


void test_null (void)
{
  fptr_array (0);
  fptr_array (&(int[0]){ });

  fstatic_array (0);                // { dg-warning "\\\[-Wnonnull" }
  fnonnull_static_array (0);        // { dg-warning "\\\[-Wnonnull" }

  fvla (0, 0);
  fnonnull_vla (0, 0);              // { dg-warning "\\\[-Wnonnull" }

  fstatic_vla (0, 0);               // { dg-warning "\\\[-Wnonnull" }
  fnonnull_static_vla (0, 0);       // { dg-warning "\\\[-Wnonnull" }
}


#pragma GCC optimize ("1")

void test_null_optimized (void)
{
  int (*pa)[0] = 0;
  fptr_array (pa);

  int *p = 0;

  fstatic_array (p);                // { dg-warning "\\\[-Wnonnull" }
  fnonnull_static_array (p);        // { dg-warning "\\\[-Wnonnull" }

  fvla (0, p);
  fnonnull_vla (0, p);              // { dg-warning "\\\[-Wnonnull" }

  fstatic_vla (0, p);               // { dg-warning "\\\[-Wnonnull" }
  fnonnull_static_vla (0, p);       // { dg-warning "\\\[-Wnonnull" }
}
