// { dg-do assemble  }
// 981203 bkoz
// g++/16567

typedef bool Bool;
typedef unsigned char		Uint8;
typedef unsigned short		Uint16;
typedef unsigned int		Uint32;

enum e_ms  { third = 3, fourth = 4 };
 
struct bitmask {
  Uint8* anon1;		 
  Uint32 anon2;
  Uint8 anon3;
  Uint8 here: 2;
  Uint8 anon4: 2;
  Uint8 anon5: 4;
};

struct control {
  Uint8 foo_1();
};

inline Uint8 foo_2(bitmask* p) {
   return p->here;
}

inline Uint8 control::foo_1() {
   return foo_2((bitmask*) this);
}

void foo(void) {
  control obj;
  control *fp = &obj;
  e_ms result;
  
  result = (e_ms) fp->foo_1; // { dg-error "" } // ERROR -
}
  




