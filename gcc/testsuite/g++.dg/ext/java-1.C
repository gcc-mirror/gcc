// { dg-do compile { target { ! { powerpc-ibm-aix* } } } }
// { dg-options "" }
// Test extern "java" and some throwing of the objects.

extern "Java"
  namespace java
  {
    namespace lang
    {
      class Throwable;
      class Class;
  }
}
typedef class java::lang::Throwable* jthrowable;
typedef class java::lang::Class* jclass;
class java::lang::Throwable {
public:
  static jclass class$;
};
int
_Jv_FindClassFromSignature ( )
  try 
    {
    }
  catch (java::lang::Throwable *ncdfe) {} 

