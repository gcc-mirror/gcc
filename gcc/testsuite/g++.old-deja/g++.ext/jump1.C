// Test that we can jump over the declaration of a non-POD object.
// Contributed by Jason Merrill <jason@cygnus.com>
// Special g++ Options: -fpermissive -w

struct A { };
union U {
  void (A::*p)();
};

int main()
{
  goto foo;
  U u;
 foo:
  return 0;
}
