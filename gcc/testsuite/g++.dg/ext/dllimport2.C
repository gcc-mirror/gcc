// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

// PR c++/9738  Dllimport attribute is overriden by later definition/redeclaration

void __attribute__((dllimport)) Bar(void);
void __attribute__((dllimport)) Baz(void);
__attribute__((dllimport)) int Biz;
__attribute__((dllimport)) int Boz;

void Foo(void)
  {
    Bar();
    Baz();
    Biz++;	 
    Boz++;	 
  }
 
void Bar(void)
  {			// { dg-warning "defined" }
  }

void Baz(void);		// { dg-warning "redeclared" }
extern int Biz;		// { dg-warning "redeclared" }
int Boz;		// { dg-warning "defined" }

void foo()
{
  Biz++;
}
