// { dg-do assemble  }
// { dg-options "-fexceptions -pedantic-errors" }
// prms-id: 6058

void bar(struct s1 { } a) { (void)a; }			// { dg-error "" } 

struct s2*fooey()
{
  try {
    static_cast<struct s3 { } *>(0);			// { dg-error "" } 
    const_cast<struct s4 { } *>((s4*)0);		// { dg-error "" } 
    reinterpret_cast<struct s5 { } *>((s3*)0);		// { dg-error "" } 
    dynamic_cast<struct s6 { } *>((s6*)0);		// { dg-error "" } 
    (struct s7 { } *)(int*)0xffedec;			// { dg-error "" } 
  } catch (struct s8 { } s) {				// { dg-error "" } 
  }
  return 0;
}
