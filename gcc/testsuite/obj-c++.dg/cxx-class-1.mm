/* Test that Objective-C++ is able to chew through a simple C++ class hierarchy.
   This was broken in earlier ObjC++ incarnations.  */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
struct foo
{
  foo(void *a) {};
};

struct bar : foo
{
  bar() : foo((char*)0) {};
};

class apple : foo
{
public:
  apple() : foo(0) { };
};
