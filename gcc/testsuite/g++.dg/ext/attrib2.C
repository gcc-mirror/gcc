// Test that an early attribute doesn't confuse uses of a class.
// { dg-do compile }

struct __attribute__ ((packed)) A
{
  void f () const;
};

void
A::f () const
{
}
