/* PR c/102103 - missing warning comparing array address to null
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __INTPTR_TYPE__  intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;

#ifndef __cplusplus
#  define bool _Bool
#endif

struct S { void *p, *a1[2], *a2[2][2]; } s, *p;

extern const void *a1[2];
extern void *a2[2][2], *ax[];

void T (bool);

void test_array_eq_0 (int i)
{
  // Verify that casts intptr_t suppress the warning.
  T ((intptr_t)a1 == 0);
  T ((uintptr_t)a1 == 0);
  T (a1 == 0);          // { dg-warning "-Waddress" }
  T (0 == &a1);         // { dg-warning "-Waddress" }
  // Verify that casts to other pointer types don't suppress it.
  T ((void *)a1 == 0);  // { dg-warning "-Waddress" }
  T ((char *)a1 == 0);  // { dg-warning "-Waddress" }
  T (a1[0] == 0);
  T (0 == (intptr_t)&a1[0]);
  T (0 == &a1[0]);      // { dg-warning "-Waddress" }
  T (a1[i] == 0);
  T (0 == (uintptr_t)&a1[i]);
  T (0 == &a1[i]);      // { dg-warning "-Waddress" }

  T ((intptr_t)a2 == 0);
  T (a2 == 0);          // { dg-warning "-Waddress" }
  T (0 == &a2);         // { dg-warning "-Waddress" }
  T (a2[0] == 0);       // { dg-warning "-Waddress" }
  T (0 == &a1[0]);      // { dg-warning "-Waddress" }
  T (a2[i] == 0);       // { dg-warning "-Waddress" }
  T (0 == &a2[i]);      // { dg-warning "-Waddress" }
  T (a2[0][0] == 0);
  T (0 == &a2[0][0]);   // { dg-warning "-Waddress" }
  T (&ax == 0);         // { dg-warning "-Waddress" }
  T (0 == &ax);         // { dg-warning "-Waddress" }
  T (&ax[0] == 0);      // { dg-warning "-Waddress" }
  T (0 == ax[0]);
}


void test_array_neq_0 (int i)
{
  // Verify that casts to intptr_t suppress the warning.
  T ((uintptr_t)a1);

  T (a1);               // { dg-warning "-Waddress" }
  T ((void *)a1);       // { dg-warning "-Waddress" }
  T (&a1 != 0);         // { dg-warning "-Waddress" }
  T (a1[0]);
  T (&a1[0] != 0);      // { dg-warning "-Waddress" }
  T (a1[i]);
  T (&a1[i] != 0);      // { dg-warning "-Waddress" }

  T ((intptr_t)a2);
  T (a2);               // { dg-warning "-Waddress" }
  T ((void *)a2);       // { dg-warning "-Waddress" }
  T ((char *)a2);       // { dg-warning "-Waddress" }
  T (&a2 != 0);         // { dg-warning "-Waddress" }
  T (a2[0]);            // { dg-warning "-Waddress" }
  T (&a1[0] != 0);      // { dg-warning "-Waddress" }
  T (a2[i]);            // { dg-warning "-Waddress" }
  T (&a2[i] != 0);      // { dg-warning "-Waddress" }
  T (a2[0][0]);
  T (&a2[0][0] != 0);   // { dg-warning "-Waddress" }
}


void test_member_array_eq_0 (int i)
{
  // Verify that casts to intptr_t suppress the warning.
  T ((intptr_t)s.a1 == 0);
  T (s.a1 == 0);        // { dg-warning "-Waddress" }
  T (0 == &a1);         // { dg-warning "-Waddress" }
  T (s.a1[0] == 0);
  T ((void*)s.a1);      // { dg-warning "-Waddress" }
  T (0 == &a1[0]);      // { dg-warning "-Waddress" }
  T (s.a1[i] == 0);
  T (0 == &a1[i]);      // { dg-warning "-Waddress" }

  T ((uintptr_t)s.a2 == 0);
  T (s.a2 == 0);        // { dg-warning "-Waddress" }
  T (0 == &a2);         // { dg-warning "-Waddress" }
  T ((void *)s.a2 == 0);// { dg-warning "-Waddress" }
  T (s.a2[0] == 0);     // { dg-warning "-Waddress" }
  T (0 == &a1[0]);      // { dg-warning "-Waddress" }
  T (s.a2[i] == 0);     // { dg-warning "-Waddress" }
  T (0 == &a2[i]);      // { dg-warning "-Waddress" }
  T (s.a2[0][0] == 0);
  T (0 == &a2[0][0]); // { dg-warning "-Waddress" }
}


void test_member_array_neq_0 (int i)
{
  // Verify that casts to intptr_t suppress the warning.
  T ((uintptr_t)s.a1);
  T (s.a1);             // { dg-warning "-Waddress" }
  T (&s.a1 != 0);       // { dg-warning "-Waddress" }
  T ((void *)&s.a1[0]); // { dg-warning "-Waddress" }
  T (s.a1[0]);
  T (&s.a1[0] != 0);    // { dg-warning "-Waddress" }
  T (s.a1[i]);
  T (&s.a1[i] != 0);    // { dg-warning "-Waddress" }

  T ((intptr_t)s.a2);
  T (s.a2);             // { dg-warning "-Waddress" }
  T (&s.a2 != 0);       // { dg-warning "-Waddress" }
  T (s.a2[0]);          // { dg-warning "-Waddress" }
  T (&s.a1[0] != 0);    // { dg-warning "-Waddress" }
  T (s.a2[i]);          // { dg-warning "-Waddress" }
  T (&s.a2[i] != 0);    // { dg-warning "-Waddress" }
  T (s.a2[0][0]);
  T (&s.a2[0][0] != 0); // { dg-warning "-Waddress" }
}
