#include <string.h>

class SomeClass_t {
public:
  SomeClass_t () : x (11) {}
protected:
  float x;
};

class DynamicOnly_t {
public:
  static DynamicOnly_t* create (const char* name = "UNDEF",
                                const SomeClass_t& somec = *(new SomeClass_t
())) {
    return new DynamicOnly_t (name, somec);
  }
  DynamicOnly_t (const char* name, const SomeClass_t& somec) :
    m_somec (somec) {
    strncpy (m_Name, name, sizeof (m_Name));
  }
private:
  SomeClass_t m_somec;
  char m_Name[255];
};

int main (int argc, char* argv[]) {
  DynamicOnly_t* ptr = DynamicOnly_t::create (); //*
  return 0;
}
