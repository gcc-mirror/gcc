void main()
{
                 class  MC{ int x; }
           const class  CC{ int x; }    static assert(is(typeof( CC.x) == const));
       immutable class  IC{ int x; }    static assert(is(typeof( IC.x) == immutable));
          shared class  SC{ int x; }    static assert(is(typeof( SC.x) == shared));
    shared const class SCC{ int x; }    static assert(is(typeof(SCC.x) == shared) && is(typeof(SCC.x) == const));

                 struct  MS{ int x; }
           const struct  CS{ int x; }   static assert(is(typeof( CS.x) == const));
       immutable struct  IS{ int x; }   static assert(is(typeof( IS.x) == immutable));
          shared struct  SS{ int x; }   static assert(is(typeof( SS.x) == shared));
    shared const struct SCS{ int x; }   static assert(is(typeof(SCS.x) == shared) && is(typeof(SCS.x) == const));

                 union  MU{ int x; }
           const union  CU{ int x; }    static assert(is(typeof( CU.x) == const));
       immutable union  IU{ int x; }    static assert(is(typeof( IU.x) == immutable));
          shared union  SU{ int x; }    static assert(is(typeof( SU.x) == shared));
    shared const union SCU{ int x; }    static assert(is(typeof(SCU.x) == shared) && is(typeof(SCU.x) == const));


                 static class  S_MC{ int x; }
           const static class  S_CC{ int x; }    static assert(is(typeof( S_CC.x) == const));
       immutable static class  S_IC{ int x; }    static assert(is(typeof( S_IC.x) == immutable));
          shared static class  S_SC{ int x; }    static assert(is(typeof( S_SC.x) == shared));
    shared const static class S_SCC{ int x; }    static assert(is(typeof(S_SCC.x) == shared) && is(typeof(S_SCC.x) == const));

                 static struct  S_MS{ int x; }
           const static struct  S_CS{ int x; }   static assert(is(typeof( S_CS.x) == const));
       immutable static struct  S_IS{ int x; }   static assert(is(typeof( S_IS.x) == immutable));
          shared static struct  S_SS{ int x; }   static assert(is(typeof( S_SS.x) == shared));
    shared const static struct S_SCS{ int x; }   static assert(is(typeof(S_SCS.x) == shared) && is(typeof(S_SCS.x) == const));

                 static union  S_MU{ int x; }
           const static union  S_CU{ int x; }    static assert(is(typeof( S_CU.x) == const));
       immutable static union  S_IU{ int x; }    static assert(is(typeof( S_IU.x) == immutable));
          shared static union  S_SU{ int x; }    static assert(is(typeof( S_SU.x) == shared));
    shared const static union S_SCU{ int x; }    static assert(is(typeof(S_SCU.x) == shared) && is(typeof(S_SCU.x) == const));
}
