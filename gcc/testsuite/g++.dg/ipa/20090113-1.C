/* { dg-do compile } */
/* { dg-options "-O3"  } */

struct S1 {
 S1() { }
};

struct S2 {
 int n;
 S1* p;
 void f() {
   p = new S1[n = 1];
 } 
};

struct S3 {
 S2 s2;
 void g() {
   s2.f();
 } 
};

void h() {
 S3().g();
}
