// { dg-do assemble  }
// GROUPS passed old-abort
// Should be fixed by:
// Sun Jun 13 12:55:22 1993  Brendan Kehoe  (brendan@lisa.cygnus.com)
// 
// 	* cp-decl.c (start_function): Avoid a null-reference on CTYPE.

template<int>
class Program {
} ;

template<>
class Program<0> {
public:
   inline friend float EvalNextArg()
        { return 1.0 ; }
} ;
