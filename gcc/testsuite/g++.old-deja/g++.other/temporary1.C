// { dg-do run  }
extern "C" int printf (const char *, ...);

int c, d;
class Foo 
{
public:
   Foo() { printf("Foo() 0x%08lx\n", (__SIZE_TYPE__)this); ++c; }
   Foo(Foo const &) { printf("Foo(Foo const &) 0x%08lx\n", (__SIZE_TYPE__)this); }
   ~Foo() { printf("~Foo() 0x%08lx\n", (__SIZE_TYPE__)this); ++d; }
};

// Bar creates constructs a temporary Foo() as a default
class Bar 
{
public:
   Bar(Foo const & = Foo()) { printf("Bar(Foo const &) 0x%08lx\n", (__SIZE_TYPE__)this); }
};

void fakeRef(Bar *)
{
}

int main() 
{
   // Create array of Bar. Will use default argument on constructor.
   // The old compiler will loop constructing Bar. Each loop will
   // construct a temporary Foo() but will not destruct the Foo(). 
   // The Foo() temporary is destructed only once after the loop 
   // completes. This could lead to a memory leak if the constructor 
   // of Foo() allocates memory.
   Bar bar[2];

   fakeRef(bar);

   printf("Done\n");

   if (c == d && c == 2)
     return 0;
   return 1;
}
