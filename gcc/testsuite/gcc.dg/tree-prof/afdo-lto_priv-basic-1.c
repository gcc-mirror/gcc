/* { dg-require-effective-target lto } */
/* { dg-additional-sources "afdo-lto_priv-basic-0.c" } */
/* { dg-options "-O2 -flto -fdump-ipa-afdo" } */
/* { dg-require-profiling "-fauto-profile" } */ 

__attribute__((noinline, noipa)) static void do_nothing() {}
__attribute__((noinline, noipa)) static void effect_1() { do_nothing(); }
__attribute__((noinline, noipa)) static void effect_2() { do_nothing(); }

void global()
{
  effect_1();
  effect_2();
}

/* Check that the annotation actually occurs.  */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for global" afdo } } */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for effect_2" afdo } } */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for effect_1" afdo } } */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for do_nothing" afdo } } */
