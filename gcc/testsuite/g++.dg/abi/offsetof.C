// Test that we can refer to the address of a base member of a null pointer
// to get its offset.  The standard says that offsetof shall not be used on
// non-POD classes, but there seems to be no such restriction on the common
// implementation thereof.

// Yes, this is bad, naughty, evil code.  But it seems to be well-formed.

// { dg-do run }

struct A { int i; };

struct B: public A {
  virtual void f ();
};

struct C: public B { };

int main ()
{
  return ((__SIZE_TYPE__) &((C*)0)->i) != sizeof(void*);
}
