// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }
//  Report error if static symbol definition has dllimport attribute.

__attribute__((dllimport))
 int impvar;			// OK,  implicit "extern"

 static __attribute__((dllimport))
 int static_impvar;	// { dg-error "external linkage" }

 static  __attribute__((dllexport))
int static_expvar;	// { dg-error "external linkage" }

static __attribute__((dllimport))
void static_impfun(void);	// { dg-error "external linkage" }

void foo()
{
  __attribute__((dllimport))
  int foovar;	// OK,  implicit "extern" 
  foovar++;
}

void bar()
{
  __attribute__((dllexport))
  int barvar;	// { dg-error "external linkage" }
  barvar++;
}
