// PR c++/18925
// { dg-do compile { target ia64-*-linux* } }
// { dg-options "-fPIC -fvisibility=hidden" }
// { dg-final { scan-assembler-not "gprel" } }

class __attribute__ ((visibility("default"))) Type 
{ 
 private: 
  static long _staticTypeCount; 
 public: 
  Type() { _staticTypeCount++; } 
  ~Type(); 
}; 
 
long Type::_staticTypeCount = 0; 
 
Type::~Type() 
{ 
 _staticTypeCount--; 
} 
