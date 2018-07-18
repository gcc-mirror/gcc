/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

#define A 5
#define B 5
#define I i
extern int a[10];
extern int g;

int
f (int i)
{
  if (i == 1) /* { dg-warning "this condition has identical branches" } */
   return a[5];
  else
   return a[5];

  if (i == 2) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
   return a[A];
  else
   return a[5];

  if (i == 3) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
   return a[5];
  else
   return a[A];

  if (i == 4) /* { dg-warning "this condition has identical branches" } */
   return a[A];
  else
   return a[A];

  if (i == 5) /* { dg-warning "this condition has identical branches" } */
   return a[i];
  else
   return a[i];

  if (i == 6) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
   return a[I];
  else
   return a[i];

  if (i == 7) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
   return a[i];
  else
   return a[I];

  if (i == 8) /* { dg-warning "this condition has identical branches" } */
   return a[I];
  else
   return a[I];

  if (i == 10) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
    g += A;
  else
    g += B;

  if (i == 11) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
    g += B;
  else
    g += A;

  if (i == 12) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
    g += A;
  else
    g += 5;

  if (i == 12) /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
    g += 5;
  else
    g += A;
}
