/* { dg-do link } */
/* { dg-require-dll "" } */
/* { dg-additional-sources "dll-7a.c" } */
/* { dg-options "-w -O2 -std=gnu99" } */

/* Test that inline functions declared "dllexport" appear in object
   files, even if they are not called.

   This behavior is required by the ARM C++ ABI:

     Exporting a function that can be inlined should force the
     creation and export of an out-of-line copy of it.

   and should presumably also apply.

   Visual Studio 2005 also honors that rule.  */

__declspec(dllexport) inline void i1() {}

__declspec(dllexport) extern inline void e1() {}

/* It is invalid to declare the function inline after its definition.  */
#if 0
__declspec(dllexport) void i2() {}
inline void i2();

__declspec(dllexport) extern void e2() {}
inline void e2();
#endif

__declspec(dllexport) inline void i3() {}
void i3();

__declspec(dllexport) inline void e3() {}
extern void e3();

__declspec(dllexport) void i4();
inline void i4() {};

__declspec(dllexport) extern void e4();
inline void e4() {};

__declspec(dllexport) inline void i5();
void i5() {};

__declspec(dllexport) inline void e5();
extern void e5() {};

/* Make sure that just declaring the function -- without defining it
   -- does not cause errors.  */
__declspec(dllexport) inline void i6();
__declspec(dllexport) extern inline void e6();
