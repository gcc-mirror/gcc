// { dg-do assemble  }
// GROUPS passed parsing
// parsing folder
// From: jonathan@Pescadero.Stanford.EDU
// Date:     Tue, 15 Sep 92 14:15:29 PDT
// Subject:  Function taking as argument a pointer to a pointer to a function
//	     that returns int causes coredump in cc1plus
// Message-ID: <9209152115.AA07423@Pescadero.Stanford.EDU>

    unsigned char FindMdc ( int (**)() );
    unsigned char FindMdc2 ( int (**funcname)() );
