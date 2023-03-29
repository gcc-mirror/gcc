// { dg-do compile { target c++14 } }

// PRs 78621

// We erroneously mangled lambda auto parms as-if template parameters (T<n>_),
// rather than auto (Da). Fixed in abi version 11

template<typename T> class X;

template<typename T>
T &&forward (T &v)
{
  return static_cast<T &&> (v);
}

template<typename T>
void eat (T &v)
{
}

template<typename S, typename T>
  void eat (S &, T &v)
{
}

inline void Foo ()
{
  auto lam = [](auto &) { };
  auto lam_1 = [](int &, auto &) { };
  auto lam_2 = [](auto &, X<int> &) { };
  auto lam_3 = [](auto (*)[5]) { };

  forward (lam);
  forward (lam_1);
  forward (lam_2);
  forward (lam_3);

  eat (lam);
  eat (lam_1);
  eat (lam_2);
  eat (lam_3);

  // The auto lambda should mangle similarly to the non-auto one
  auto lambda_1 = [](float *, float *) { };
  auto lambda_2 = [](auto *, auto *) { };
  auto lambda_3 = [](auto *, auto *) { };

  int *i;
  
  eat (i, lambda_1);
  eat (i, lambda_2);

  // The autos should squangle to the first one.
  eat (lambda_2, lambda_3);
}

template<typename X> void Bar ()
{
  auto lambda_1 = [](X *, float *, float *) { };
  auto lambda_2 = [](X *, auto *, auto *) { };
  auto lambda_3 = [](X *, auto *...) {};
  
  int *i;
  
  eat (i, lambda_1);
  eat (i, lambda_2);
  eat (i, lambda_3);
}

void Baz ()
{
  Bar<short> ();
  Foo ();
}

// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRT_E_EOS0_S1_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRiRT_E0_EOS1_S2_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlRT_R1XIiEE1_EOS0_S1_:" } }
// { dg-final { scan-assembler "_Z7forwardIZ3FoovEUlPA5_T_E2_EOS0_RS0_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRT_E_EvS1_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRiRT_E0_EvS2_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlRT_R1XIiEE1_EvS1_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlPA5_T_E2_EvRS0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3FoovEUlPfS1_E3_EvRT_RT0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3FoovEUlPT_PT0_E4_EvRS1_RS3_:" } }
// { dg-final { scan-assembler "_Z3eatIZ3FoovEUlPT_PT0_E4_Z3FoovEUlS1_S3_E5_EvRS0_RS2_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsPfS3_E_EvRT_RT0_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsPT_PT0_E0_EvRS3_RS5_:" } }
// { dg-final { scan-assembler "_Z3eatIPiZ3BarIsEvvEUlPsDpPT_E1_EvRT_RT0_:" } }
