/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu" } */

extern int get_cordz_mean_interval ();
extern thread_local long cordz_next_sample, kIntervalIfDisabled;
extern bool cordz_should_profile_slow (void);
inline bool
cordz_should_profile (void)
{
  return cordz_should_profile_slow ();
}
bool
cordz_should_profile_slow (void)
{
  int mean_interval = get_cordz_mean_interval ();
  if (mean_interval)
    cordz_next_sample = kIntervalIfDisabled;
  return cordz_next_sample || cordz_should_profile ();
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 2 { target { ! ia32 } } } } */
