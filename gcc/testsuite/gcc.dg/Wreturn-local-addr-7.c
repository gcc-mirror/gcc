/* Test to verify that a PHI with a COND_EXPR argument in a return
   statement is handled correctly.
  { dg-do compile }
  { dg-options "-O2 -Wall" } */

extern struct S s;

void* f (int n)
{
  void *p;
  int x = 0;

  for (int i = n; i >= 0; i--)
    {
      p = &s;
      if (p == (void*)-1)
         x = 1;
      else if (p)
         return p;
    }

  /* The return statement below ends up with the following IL:
     <bb 6> [local count: 59055800]:
     # x_10 = PHI <1(5), 0(2)>
     _5 = x_10 != 0 ? -1B : 0B;

     <bb 7> [local count: 114863532]:
     # _3 = PHI <&s(4), _5(6), &s(3)>
     return _3;  */
  return x ? (void*)-1 : 0;
}

void* g (int n)
{
  void *p;
  int x = 0;                  /* { dg-message "declared here" } */

  for (int i = n; i >= 0; i--)
    {
      p = &s;
      if (p == (void*)-1)
         x = 1;
      else if (p)
         return p;
    }

  /* The return statement below does not reference a COND_EXPR argument.  */
  return x ? &x : 0;          /* { dg-warning "may return address of local variable" "missing location" { xfail *-*-* } } */
  /* { dg-warning "may return address of local variable" "pr90735" { target *-*-* } 0 } */
}
