// { dg-do assemble  }
// Origin: Loring Holden <lsh@lsh.cs.brown.edu>

template <class T>
class REFptr {
   public:
      operator T* () const;
};

class CamFocus;
typedef REFptr<CamFocus> CamFocusptr;

class CamFocus {
   protected:
      static CamFocusptr  _focus;
   public :
      static CamFocusptr &cur() { return _focus; }
};

void
test()  
{
   if (CamFocus::cur()) {
   }
}
