// Build don't link: 
// GROUPS passed missed-error
// missed-error file
// From: John Carr <jfc@Athena.MIT.EDU>
// Date:     Tue, 02 Feb 1993 07:38:53 EST
// Subject:  Re: g++ ignores language context of function pointers
// Message-ID: <9302021238.AA01513@Achates.MIT.EDU>

        typedef void (*pfv2)(double, double);
        extern "C" { typedef void (*pfv3)(double, double); }// ERROR -  , XFAIL *-*-*

