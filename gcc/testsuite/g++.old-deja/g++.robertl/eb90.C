// Special g++ Options: -O2
// Build don't link: 

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
