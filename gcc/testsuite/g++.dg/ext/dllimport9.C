//  Handle dllimport attribute for functions declared inline.
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }
// { dg-options { -W } }

inline __attribute__((dllimport)) void bar() { }	// { dg-warning "inline" }

struct __attribute__ ((dllimport)) Blah	
{
  void in_blah () { }				// { dg-warning "inline" }
  void out_blah ();
};

inline void Blah::out_blah(){ }			// { dg-warning "inline" }

void use_inlines()
{
  Blah aBlah;
  bar();
  aBlah.in_blah ();
  aBlah.out_blah ();
}

// { dg-final { scan-assembler-not "__imp__" } }
