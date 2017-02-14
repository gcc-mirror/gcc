// PR c++/33459
// { dg-prune-output "uninitialized" }
// { dg-prune-output "deleted" }

union A
{
  int &i; // { dg-error "may not have reference type" }
};

void foo()
{
  A();
}
