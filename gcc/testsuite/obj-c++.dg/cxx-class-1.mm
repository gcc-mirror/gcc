/* Test that Objective-C++ is able to chew through a simple C++ class hierarchy.
   This was broken in earlier ObjC++ incarnations.  */

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
