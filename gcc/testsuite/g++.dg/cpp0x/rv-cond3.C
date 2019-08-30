// PR c++/88103
// { dg-do compile { target c++11 } }

struct A {
  A (int);
  A&& foo () &&;
  int i;
};
void free (A&&);

void test_xvalue (A a){
  A&& ref = true ? static_cast<A&&> (a) : static_cast<A&&> (a); 
  free (true ? static_cast<A&&> (a) : static_cast<A&&> (a));
  (true ? static_cast<A&&> (a) : static_cast<A&&> (a)).foo ();
  int&& k = (true ? static_cast<A&&> (a) : static_cast<A&&> (a)).i;
}
void test_prvalue (A a){
  A&& ref = true ? static_cast<A&&> (a) : 1; 
  free (true ? static_cast<A&&> (a) : 1);
  (true ? static_cast<A&&> (a) : 1).foo ();
  int&& k = (true ? static_cast<A&&> (a) : 1).i;
}
