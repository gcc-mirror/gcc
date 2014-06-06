// PR c++/33459
// { dg-prune-output "uninitialized" }
// { dg-prune-output "deleted" }

union A
{
  int &i; // { dg-error "may not have reference type" "" { target { ! c++11 } } }
};

void foo()
{
  A();
}
