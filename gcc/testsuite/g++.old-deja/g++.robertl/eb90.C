// { dg-do assemble  }
// { dg-options "-O2" }

#ifdef HIDE_BUG
#define realloc Realloc
#endif

class TmpRgn {
public:
     void
   realloc();
};

class TmpActor {
    TmpRgn      tmpRgn;

public:
      void
      realloc() {
          tmpRgn.realloc();
      }
};
