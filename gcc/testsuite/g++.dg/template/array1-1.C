// { dg-do compile }
// { dg-options "-fabi-version=1" }

// Contributed by Nathan Sidwell 22 Dec 2003 <nathan@codesourcery.com>
// Origin: Roger Sayle  <roger@eyesopen.com>

// PR c++/12774 Array domains compared unequal

void Foo(double r[3][3])
{
}

void Baz()
{
   double m[3][3];
   Foo(m);
}

template <class T>
void Bar()
{
   double m[3][3];
   Foo(m);
}

int main()
{
   Baz();
   Bar<int>();
   return 0;
}

