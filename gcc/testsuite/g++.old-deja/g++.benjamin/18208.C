// 981204 bkoz
// g++/18208
// Build don't link:

typedef unsigned int uint_32;

class puertorico {
public:
  void *f ();
private:
  uint_32 member;
};

void foo( )
{
  uint_32 ui;
  puertorico obj;

  // Bug using static_cast<>
  ui = static_cast<uint_32>(obj); // ERROR - // ERROR -
  
  // Bug when missing the pair of braces
  ui = (uint_32) obj.f; // ERROR - // ERROR -
}

