// { dg-do assemble  }
// GROUPS passed pointers-to-members
struct C {
  struct N {
    int g1(int);
    int g2(int);
  };

  typedef int (N::*pmfn)(int);
  
  int f1(int);
  int f2(int);
};

int (C::*f)(int) = &C::f1;		

/*
   The compiler should not crash on the line below; this change fixes it:
        * cp-tree.c (list_hash_lookup_or_cons): Make sure the type doesn't
        have TYPE_PTRMEMFUNC_P set before we try to build its
        CLASSTYPE_ID_AS_LIST.
*/

C::pmfn h = &C::N::g1;			

int (C::N::*g)(int) = &C::N::g2;	
