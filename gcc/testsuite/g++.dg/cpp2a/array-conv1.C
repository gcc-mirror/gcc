// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++17 } }
// { dg-options "-Wpedantic" }
// C++17, because that has CWG 393.

void f(int(&)[]);
void fp(int(*)[]);
void f2(int(&)[][10]);
void fp2(int(*)[][10]);
int arr[10];
int arr2[10][10];

void
g ()
{
  f (arr); // { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
  fp (&arr); // { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
  f2 (arr2);// { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
  fp2 (&arr2);// { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
}

int(&r1)[] = arr;// { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
int(&r2)[10] = arr;
int(&r3)[][10] = arr2;// { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
/* Note that
   int (&r)[10][] = arr2;
   is invalid.  */
int(&r4)[10][10] = arr2;

int(*p1)[] = &arr;// { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
int(*p2)[10] = &arr;
int(*p3)[][10] = &arr2;// { dg-warning "conversions to arrays of unknown bound are only available" "" { target c++17_down } }
int(*p4)[10][10] = &arr2;
