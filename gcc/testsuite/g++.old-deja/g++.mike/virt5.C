// Ensure that virtual base upcast and downcasting works on this
// conversions during virtual function dispatch at ctor/dtor time
// when dynamic vtable fixups for deltas are needed.

int fail = 0;

struct BASE1 {
  virtual ~BASE1 () { }      
};   

class MID;

class BASE2 {
public: 
  virtual MID *VFN (){ return 0; }
};   

class MIBASE : public BASE1, public BASE2 { };   

class VBB : public MIBASE {
public:
  virtual long get_STATE () const = 0;
  void print_STATE() { if (get_STATE () != 87654321) fail = 1; }
};   

class VBD : public virtual VBB {
  long STATE;
public:
  long get_STATE() const { return STATE; }
  VBD() { STATE = 87654321; }
  ~VBD() { STATE = 87654321; }
};   

class MID : public virtual VBD {
public:
  MID () { print_STATE(); }
  ~MID () { print_STATE(); }
  virtual MID *VFN() { return this; }
};  

class LAST : public MID {
public:
  LAST () { print_STATE(); }
  ~LAST () { print_STATE(); }
};

int main() {
  MIBASE *o = new LAST;
  MID *p = o->VFN();
  p->print_STATE();
  delete o;
  return fail;
}
