// { dg-do assemble  }
// GROUPS passed missed-error
// missed-error file
// From: John Carr <jfc@Athena.MIT.EDU>
// Date:     Tue, 02 Feb 1993 07:38:53 EST
// Subject:  Re: g++ ignores language context of function pointers
// Message-ID: <9302021238.AA01513@Achates.MIT.EDU>

typedef void (*pfv)();
void f ();

extern "C"
{
  typedef void (*pcfv)(void);
  void cf (void);
}

pfv p = f;
pfv p2 = cf;			// { dg-error "" "" { xfail *-*-* } } mismatch 
pcfv p3 = f;			// { dg-error "" "" { xfail *-*-* } } mismatch 
pcfv p4 = cf;
