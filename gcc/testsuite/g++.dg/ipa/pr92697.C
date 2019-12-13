/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -fdump-ipa-sra" } */

extern int have_avx2;
extern int have_ssse3;

namespace NTL
{

  static void randomstream_impl_init_base ()
  {
    __builtin_printf ("Frob1\n");
  }

  static void // __attribute__ ((target ("ssse3")))
    randomstream_impl_init_ssse3 ()
  {
    __builtin_printf ("Frob2\n");
  }

  static void
    //__attribute__ ((target ("avx2,fma,avx,pclmul,ssse3")))
    randomstream_impl_init_avx2 ()
  {
    __builtin_printf ("Frob3\n");
  }

  extern "C"
  {
    static void (*resolve_randomstream_impl_init (void)) ()
    {
      if (have_avx2)
	return &randomstream_impl_init_avx2;
      if (have_ssse3)
	return &randomstream_impl_init_ssse3;
      return &randomstream_impl_init_base;
    }
  }
  static void
    __attribute__ ((ifunc ("resolve_" "randomstream_impl_init")))
    randomstream_impl_init ();
  void foo ()
  {
    randomstream_impl_init ();
  }

}


/* { dg-final { scan-ipa-dump-not "Created new node" "sra" } } */
