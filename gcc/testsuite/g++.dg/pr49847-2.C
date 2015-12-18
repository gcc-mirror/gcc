/* { dg-do compile { target m68k-*-* } } */
/* { dg-options "-O2 -mcpu=68060 -fnon-call-exceptions -fPIC -O2 -fpermissive" } */

extern "C" {
    typedef __java_int jint;
    typedef __java_float jfloat;
    namespace java   {
     namespace lang     {
       class Class;
       class Object;
       class Throwable;
   }
}
  }
  typedef class   java::lang::Class *   jclass;
  typedef   class   java::lang::Throwable *   jthrowable;
  typedef unsigned short _Jv_ushort __attribute__ ((__mode__ (__HI__)));
  extern   "Java" {
    struct _JvObjectPrefix   {
   };
  }
  class   java::lang::Object:   public   _JvObjectPrefix {
  };
   union _Jv_word {
    jint     i;
    jfloat     f;
  };
  class _Jv_MethodBase {
  };
   class _Jv_InterpMethod: public _Jv_MethodBase {
  private:_Jv_ushort max_stack;
    static void   run (_Jv_InterpMethod *);
  };
   class java::lang::Throwable: public::java::lang::Object {
  public:static::java::lang::Class     class$;
  };
   void _Jv_InterpMethod::run (_Jv_InterpMethod * meth) {
    _Jv_word stack[meth->max_stack];
    _Jv_word *sp = stack;
    try   {
     jfloat value2 = ((jfloat) (--sp)->f);
     jfloat value1 = ((jfloat) (--sp)->f);
     if (value1 > value2)       (sp++)->i = (1);
   }
    catch (java::lang::Throwable * ex)   {
   }
  }
