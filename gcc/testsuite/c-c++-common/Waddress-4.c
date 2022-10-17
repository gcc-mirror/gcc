/* PR c/102103 - missing warning comparing array address to null
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __INTPTR_TYPE__ intptr_t;
typedef __INTPTR_TYPE__ uintptr_t;

extern char *ax[], *a2[][2];

void T (int);

void test_ax_plus_eq_0 (int i)
{
  // Verify that casts to intptr_t suppress the warning.
  T ((intptr_t)(ax + 0) == 0);
  T ((uintptr_t)(ax + 1) == 0);

  T (ax + 0 == 0);      // { dg-warning "-Waddress" }
  T (&ax[0] == 0);      // { dg-warning "-Waddress" }
  T (ax - 1 == 0);      // { dg-warning "-Waddress" }
  T (0 == &ax[-1]);     // { dg-warning "-Waddress" }
  T ((void *)(&ax[0] + 2) == 0);  // { dg-warning "-Waddress" }
  T (&ax[0] + 2 == 0);  // { dg-warning "-Waddress" }
  T (ax + 3 == 0);      // { dg-warning "-Waddress" }
  T (0 == &ax[-4]);     // { dg-warning "-Waddress" }
  T (ax - i == 0);      // { dg-warning "-Waddress" }
  T (&ax[i] == 0);      // { dg-warning "-Waddress" }
  T (0 == &ax[1] + i);  // { dg-warning "-Waddress" }
}

void test_a2_plus_eq_0 (int i)
{
  // Verify that casts to intptr_t suppress the warning.
  T ((intptr_t)(a2 + 0) == 0);
  T ((uintptr_t)(a2 + 1) == 0);

  T (a2 + 0 == 0);      // { dg-warning "-Waddress" }
  // Verify that a cast to another pointer type doesn't suppress it.
  T ((void*)(a2 + 0) == 0);       // { dg-warning "-Waddress" }
  T ((char*)a2 + 1 == 0);         // { dg-warning "-Waddress" }
  T (&a2[0] == 0);      // { dg-warning "-Waddress" }
  T (a2 - 1 == 0);      // { dg-warning "-Waddress" }
  T (0 == &a2[-1]);     // { dg-warning "-Waddress" }
  T (a2 + 2 == 0);      // { dg-warning "-Waddress" }
  T (0 == &a2[-2]);     // { dg-warning "-Waddress" }
  T (a2 - i == 0);      // { dg-warning "-Waddress" }
  T (&a2[i] == 0);      // { dg-warning "-Waddress" }
}

// Exercise a pointer.
void test_p_plus_eq_0 (int *p, int i)
{
  /* P + 0 and equivalently &P[0] are invalid for a null P but they're
     folded to p before the warning has a chance to trigger.  */
  T (p + 0 == 0);       // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }
  T (&p[0] == 0);       // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }

  T (p - 1 == 0);       // { dg-warning "-Waddress" }
  T (0 == &p[-1]);      // { dg-warning "-Waddress" }
  T (p + 2 == 0);       // { dg-warning "-Waddress" }
  T (0 == &p[-2]);      // { dg-warning "-Waddress" }
  T (p - i == 0);       // { dg-warning "-Waddress" }
  T (&p[i] == 0);       // { dg-warning "-Waddress" }
}

// Exercise pointer to array.
void test_pa_plus_eq_0 (int (*p)[], int (*p2)[][2], int i)
{
  // The array pointer may be null.
  T (*p == 0);
  /* &**P is equivalent to *P and might be the result od macro expansion.
     Verify it doesn't cause a warning.  */
  T (&**p == 0);

  /* *P + 0 is invalid but folded to *P before the warning has a chance
     to trigger.  */
  T (*p + 0 == 0);      // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }

  T (&(*p)[0] == 0);    // { dg-warning "-Waddress" }
  T (*p - 1 == 0);      // { dg-warning "-Waddress" }
  T (0 == &(*p)[-1]);   // { dg-warning "-Waddress" }
  T (*p + 2 == 0);      // { dg-warning "-Waddress" }
  T (0 == &(*p)[-2]);   // { dg-warning "-Waddress" }
  T (*p - i == 0);      // { dg-warning "-Waddress" }
  T (&(*p)[i] == 0);    // { dg-warning "-Waddress" }


  /* Similar to the above but for a pointer to a two-dimensional array,
     referring to the higher-level element (i.e., an array itself).  */
  T (*p2 == 0);
  T (**p2 == 0);         // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }
  T (&**p2 == 0);        // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }
  T (&***p2 == 0);       // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }
  T (&**p2 == 0);

  T (*p2 + 0 == 0);      // { dg-warning "-Waddress" "pr102555" { xfail *-*-* } }
  T (&(*p2)[0] == 0);    // { dg-warning "-Waddress" }
  T (&(*p2)[0][1] == 0); // { dg-warning "-Waddress" }
  T (*p2 - 1 == 0);      // { dg-warning "-Waddress" }
  T (0 == &(*p2)[-1]);   // { dg-warning "-Waddress" }
  T (0 == &(*p2)[1][2]); // { dg-warning "-Waddress" }
  T (*p2 + 2 == 0);      // { dg-warning "-Waddress" }
  T (0 == &(*p2)[-2]);   // { dg-warning "-Waddress" }
  T (*p2 - i == 0);      // { dg-warning "-Waddress" }
  T (&(*p2)[i] == 0);    // { dg-warning "-Waddress" }
}
