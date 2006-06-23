//  Report error if dllimport attribute in definition itself.
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

__attribute__((dllimport))  void bar () { }	// { dg-error "definition" }

__attribute__((dllimport))  int foo = 1;	// { dg-error "definition" }

void faz()
{
  __attribute__((dllimport)) int faa = 1;	// { dg-error "definition" }
  faa++; 
}

__attribute__((dllimport)) int fee (1);		// { dg-error "definition" }


// In-class initialization of a static data member is not a definition.  
struct  F
{
  __attribute__ ((dllimport)) static const int i = 1;  // OK
};

// Reference the dllimport'd static data member.
void f ()
{
  const int* j = &F::i;
}

struct  G
{
  __attribute__ ((dllimport)) static const int i = 1;
};

// Define the static data member _without_ the dllimport.
// This should override the prior declaration with dllimport.

const int G::i;		//  { dg-warning "dllimport ignored" }

