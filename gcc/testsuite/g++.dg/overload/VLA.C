//Origin: kengole@us.ibm.com

//PR c++/2478
// G++ was rejecting this as it could not convert `int (*)[]' to `int (*)[0]'.
// Using the C99 VLA style arrays in a struct.

// { dg-do compile }

struct S {
  int (*p)[];
} B;

void foo() {
  int (*p)[];
  B.p=p;  // { dg-bogus "cannot convert" }
}
