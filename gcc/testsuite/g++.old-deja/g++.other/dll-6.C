// { dg-do assemble { target arm-*-*pe } }
// set not_compiler_result "__imp_"
// dll.h
class aClass 
    { 
public: 
    __declspec(dllimport) aClass(); 
    }; 

// dll.cpp

__declspec(dllexport) aClass::aClass() 
    { 
    } 
