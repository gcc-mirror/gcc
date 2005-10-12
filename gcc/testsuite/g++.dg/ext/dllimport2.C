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

void Baz(void);		// { dg-warning "referenced with dll linkage" }
void Bar(void)		// { dg-warning "referenced with dll linkage" }
  {
  }
extern int Biz;		// { dg-warning "referenced with dll linkage" }
int Boz;		// { dg-warning "referenced with dll linkage" }

void foo()
{
  Biz++;
}
