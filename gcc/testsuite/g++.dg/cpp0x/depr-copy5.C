// PR c++/106213
// { dg-additional-options -Werror=deprecated-copy-dtor }
// { dg-prune-output "warnings being treated as errors" }

struct s {
  int* i;
  s();
  ~s() { delete i; }
};


void bar(){
  s instance;
  s instance2 = instance;
  // { dg-error "deprecated-copy-dtor" "" { target c++11 } .-1 }
}
