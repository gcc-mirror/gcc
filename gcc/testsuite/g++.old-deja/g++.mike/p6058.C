// Build don't link:
// Special g++ Options: -fexceptions -pedantic-errors
// prms-id: 6058

void bar(struct s1 { } a) { (void)a; }			// ERROR - 

struct s2*fooey()
{
  try {
    static_cast<struct s3 { } *>(0);			// ERROR - 
    const_cast<struct s4 { } *>((s4*)0);		// ERROR - 
    reinterpret_cast<struct s5 { } *>((s3*)0);		// ERROR - 
    dynamic_cast<struct s6 { } *>((s6*)0);		// ERROR - 
    (struct s7 { } *)(int*)0xffedec;			// ERROR - 
  } catch (struct s8 { } s) {				// ERROR - 
  }
  return 0;
}
