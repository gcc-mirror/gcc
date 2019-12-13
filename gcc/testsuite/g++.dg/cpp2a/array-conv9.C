// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }

int arr[1];
extern int arr2[];

void
test ()
{
  int (&r)[1] = const_cast<int(&)[1]>(arr);
  int (&r2)[] = const_cast<int(&)[]>(arr); // { dg-error "17:invalid .const_cast." }
  int (&r3)[1] = (int(&)[1]) arr;
  int (&r4)[] = (int(&)[]) arr;
  int (&r5)[1] = static_cast<int(&)[1]>(arr);
  int (&r6)[] = static_cast<int(&)[]>(arr);

  // Try c_cast_p.
  int(*p1)[] = (int(*)[]) &arr;
  int(*p2)[1] = (int(*)[]) &arr; // { dg-error "cannot convert" }
  int(*p3)[] = (int(*)[1]) &arr;
  int(*p4)[] = (int(*)[1]) &arr2;
  int(*p5)[] = (int(*)[]) (int(*)[1]) &arr;
  int(*p6)[] = (int(*)[1]) (int(*)[]) &arr;
  int(*p7)[] = static_cast<int(*)[]>(&arr);
  int(*p8)[] = static_cast<int(*)[1]>(&arr);
  int(*p9)[] = static_cast<int(*)[1]>(&arr2); // { dg-error "16:invalid .static_cast." }
}
