// Build don't link:
// Skip if not target: arm-*pe
// Special g++ Options: -mno-nop-fun-dllimport
// set compiler_result "__imp_f1.*\.section${spaces}.drectve\n\[^\n\]*-export:f2"
// set not_compiler_result "__imp_f2"

class aClass 
{ 
public: 
  __declspec(dllimport) int f1(); 
  __declspec(dllexport) int f2(); 
}; 
 
__declspec(dllexport) int aClass::f2() 
{ 
  return f1(); 
} 
