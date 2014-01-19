/* { dg-do compile } */
/* { dg-options "-O2 -finstrument-functions" } */
/* { dg-additional-options "-mno-explicit-relocs" { target alpha*-*-* } } */
/* { dg-final { scan-assembler-times "__cyg_profile_func_enter" 1} } */

#define NOINSTR __attribute__((no_instrument_function))

struct t
{
   public:
       /* Function code should be instrumented */
       __attribute__((noinline)) t() {}

       /* Function t::a() should not be instrumented */
       NOINSTR void a(){
       }
       /* Function t::b() should not be instrumented */
       void NOINSTR b(){
       }
       /* Function t::c() should not be instrumented */
       void c() NOINSTR {
       }
       /* Function t::d() should not be instrumented */
       void d() NOINSTR;
};

void t::d()
{
}

/* Function call_all_functions() should not be instrumented */
struct t call_all_functions() __attribute__((no_instrument_function));
struct t call_all_functions()
{
       struct t a;     /* Constructor not inlined */
       a.a();	       /* Inlined t::a() should not be instrumented */
       a.b();	       /* Inlined t::b() should not be instrumented */
       a.c();	       /* Inlined t::c() should not be instrumented */
       a.d();	       /* Inlined t::d() should not be instrumented */
       return a;
}

