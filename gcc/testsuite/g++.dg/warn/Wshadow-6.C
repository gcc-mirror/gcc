// Test the declaration of nested lambda function shadows
// a parameter or previous local.
// { dg-do compile { target c++11 } }
// { dg-options "-Wshadow" }

struct S {};
int f1(int x)   // { dg-message "shadowed declaration" }
{
 int t = 0;
 int m = 0;     // { dg-message "shadowed declaration" }
 [&t] (int x) { // { dg-warning "shadows a parameter" }
   int m = 1;   // { dg-warning "shadows a previous local" }
   t = t + x + m;
 };
 return t;
}

void f2(struct S i, int j) {
  struct A {
    struct S x;
    void g(struct S i) { // { dg-message "shadowed declaration" }
	  struct S x;    // { dg-warning "shadows a member of" }
	  struct S y;    // { dg-message "shadowed declaration" }
	  int t;
	   [&t](struct S i){   // { dg-warning "shadows a parameter" }
		 int j = 1;    // { dg-bogus "shadows" }
		 struct S y;   // { dg-warning "shadows a previous local" }
 		 t = j;
	   };
    }
  };
}

void f3(int i) {
 [=]{
   int j = i;			// { dg-message "shadowed declaration" }
   int i;			// { dg-warning "shadows a lambda capture" }
   i = 1;
 };
}

template <class T>
void f4(int i) {
 [=]{
   int j = i;			// { dg-message "shadowed declaration" }
   int i;			// { dg-warning "shadows a " }
   i = 1;
 };
}

template void f4<int>(int);
