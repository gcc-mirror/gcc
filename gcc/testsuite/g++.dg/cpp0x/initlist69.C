// PR c++/50261
// { dg-require-effective-target c++11 }

template <typename T>
struct ca {
  T elem[1];

  ca(const T (&s)[1]): elem{{s}} { }	   // { dg-error "invalid" }
  ca(const T (&s)[1],int): elem({{s}}) { } // { dg-error "paren|invalid" }
  ca(const T (&s)[1],char): elem(s) { }	   // { dg-error "array" }
  ca(const T (&s)[1],double): elem{s} { }  // { dg-error "invalid" }

  ca(const T &v): elem{{v}} { }	      // OK
  ca(const T &v,int): elem{{{v}}} { } // { dg-error "braces" }
  ca(const T &v,char): elem{v} { }    // OK
  ca(const T &v,double): elem({v}) { } // { dg-error "paren" }
};

int main() {
  int a[1] = {0};
  ca<int> d(a);
  ca<int> d1(a,1);
  ca<int> d2(a,'2');
  ca<int> d3(a,3.0);
  ca<int> e(a[0]);
  ca<int> e1(a[0],1);
  ca<int> e2(a[0],'2');
  ca<int> e3(a[0],3.0);
}
