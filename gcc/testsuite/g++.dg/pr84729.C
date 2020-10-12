// { dg-do compile }
// { dg-options "-fpermissive" }

typedef int b[2];
void a() {
  new b(a); // { dg-error "parenthesized initializer in array new|invalid conversion" }
}
