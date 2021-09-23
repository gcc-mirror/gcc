/* PR middle-end/91458 - inconsistent warning for writing past the end
   of an array member
   Verify that the -Wstringop-overflow detection doesn't cause an ICE
   for either kind of VLAs (member and non-member).
   Diagnosing the accesses is the subject of pr82608.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" }
   { dg-require-effective-target alloca } */

void sink (void*);

void vla_unbounded (int n)
{
  char a[n];

  a[0] = 0;
  a[1] = 1;
  a[n] = n;         // { dg-warning "\\\[-Wstringop-overflow" "pr82608" { xfail *-*-* } }

  sink (&a);
}

void vla_bounded (int n)
{
  if (n > 32)
    n = 32;

  char a[n];

  a[0] = 0;
  a[1] = 1;
  a[n] = n;         // { dg-warning "\\\[-Wstringop-overflow" "pr82608" { xfail *-*-* } }
  a[69] = n;        // { dg-warning "\\\[-Wstringop-overflow" "pr82608" }

  sink (&a);
}


void member_vla_unbounded (int n)
{
  struct S { char i, a[n]; } s;

  s.a[0] = 0;
  s.a[1] = 1;
  s.a[n] = n;       // { dg-warning "\\\[-Wstringop-overflow" "pr82608" { xfail *-*-* } }

  sink (&s);
}

void member_vla_bounded (int n)
{
  if (n > 32)
    n = 32;

  struct S { char i, a[n]; } s;

  s.a[0] = 0;
  s.a[1] = 1;
  s.a[n] = n;       // { dg-warning "\\\[-Wstringop-overflow" "pr82608" { xfail *-*-* } }
  s.a[69] = n;      // { dg-warning "\\\[-Wstringop-overflow" "pr82608" { xfail *-*-* } }

  sink (&s);
}
