// This testcase used to crash while looking in A for my_module.  I'm still
// not sure it's well-formed, but it works now because of the optimization
// to look at the expected address first.

// { dg-do run }

extern "C" int puts (const char *);
extern "C" void abort ();

struct my_object
{
  my_object() { puts ("in my_object ctor");}
  virtual ~my_object() { puts ("in my_object dtor"); }
};

my_object* my_module_ptr = 0;

struct my_module : my_object
{
  my_module()
  {
    puts ("in my_module ctor, setting up ptr");
    my_module_ptr = this;
  }
  ~my_module() { puts ("in my_module dtor");}
};

struct D
{
  D() { puts ("in D ctor"); }
  virtual ~D();
};

D::~D()
{
  puts ("in D dtor");
  puts ("before DCASTing to my_module*");
  my_module* m = dynamic_cast<my_module*>(my_module_ptr);
  if (m != my_module_ptr)
    abort ();
  puts ("after DCASTing to my_module*");
}

struct my_interface
{
  my_interface() { puts ("in my_interface ctor");}
  ~my_interface() { puts ("in my_interface dtor");}
};

struct myif : virtual my_interface
{
  myif() { puts ("in myif ctor");}
  ~myif() { puts ("in myif dtor");}
};

struct A: virtual myif
{
  A() { puts ("in A ctor"); }
  ~A() { puts ("in A dtor"); }

  D d;
};

struct B: virtual myif
{
  B() { puts ("in B ctor"); }
  ~B() { puts ("in B dtor"); }

  D d;
};

struct C : my_module, A, B
{
  C() { puts ("in C ctor");}
  ~C() { puts ("in C dtor"); }
};

int main(int, char**)
{
  C t;
}
