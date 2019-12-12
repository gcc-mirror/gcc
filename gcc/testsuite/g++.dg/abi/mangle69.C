// { dg-do compile { target c++2a } }

struct A1 { char c[5]; };

template <A1> struct B { };

// All of the following name the same type.
typedef B<A1{ }>                   A______;
typedef B<A1{ { 0 } }>             A_Z____;
typedef B<A1{ { 0, 0 } }>          A_ZZ___;
typedef B<A1{ { 0, 0, 0 } }>       A_ZZZ__;
typedef B<A1{ { 0, 0, 0, 0 } }>    A_ZZZZ_;
typedef B<A1{ { 0, 0, 0, 0, 0 } }> A_ZZZZZ;

// Verify that the types mangle the same.
void a______ (A______) { }
// { dg-final { scan-assembler "_Z7a______1BIXtl2A1EEE" } }

void a_z____ (A_Z____) { }
// { dg-final { scan-assembler "_Z7a_z____1BIXtl2A1EEE" } }

void a_zz___ (A_ZZ___) { }
// { dg-final { scan-assembler "_Z7a_zz___1BIXtl2A1EEE" } }

void a_zzz__ (A_ZZZ__) { }
// { dg-final { scan-assembler "_Z7a_zzz__1BIXtl2A1EEE" } }

void a_zzzz_ (A_ZZZZ_) { }
// { dg-final { scan-assembler "_Z7a_zzzz_1BIXtl2A1EEE" } }

void a_zzzzz (A_ZZZZZ) { }
// { dg-final { scan-assembler "_Z7a_zzzzz1BIXtl2A1EEE" } }


// All of the following use a string to initialize the array but
// also name the same type as the above.
typedef B<A1{ "" }>                S_z____;
typedef B<A1{ "\0" }>              S_Zz___;
typedef B<A1{ "\0\0" }>            S_ZZz__;
typedef B<A1{ "\0\0\0" }>          S_ZZZz_;
typedef B<A1{ "\0\0\0\0" }>        S_ZZZZz;

// Verify that the types mangle the same.
void s_z____ (S_z____) { }
// { dg-final { scan-assembler "_Z7s_z____1BIXtl2A1EEE" } }

void s_Zz___ (S_Zz___) { }
// { dg-final { scan-assembler "_Z7s_Zz___1BIXtl2A1EEE" } }

void s_ZZz__ (S_ZZz__) { }
// { dg-final { scan-assembler "_Z7s_ZZz__1BIXtl2A1EEE" } }

void s_ZZZz_ (S_ZZZz_) { }
// { dg-final { scan-assembler "_Z7s_ZZZz_1BIXtl2A1EEE" } }

void s_ZZZZz (S_ZZZZz) { }
// { dg-final { scan-assembler "_Z7s_ZZZZz1BIXtl2A1EEE" } }


// All of the following also name the same type (distinct from
// the above).
typedef B<A1{ { 'A' } }>              A_A____;
typedef B<A1{ { 'A', 0 } }>           A_AZ___;
typedef B<A1{ { 'A', 0, 0 } }>        A_AZZ__;
typedef B<A1{ { 'A', 0, 0, 0 } }>     A_AZZZ_;
typedef B<A1{ { 'A', 0, 0, 0, 0 } }>  A_AZZZZ;

void a_A____ (A_A____) { }
// { dg-final { scan-assembler "_Z7a_A____1BIXtl2A1tlA5_cLc65EEEEE" } }

void a_AZ___ (A_AZ___) { }
// { dg-final { scan-assembler "_Z7a_AZ___1BIXtl2A1tlA5_cLc65EEEEE" } }

void a_AZZ__ (A_AZZ__) { }
// { dg-final { scan-assembler "_Z7a_AZZ__1BIXtl2A1tlA5_cLc65EEEEE" } }

void a_AZZZ_ (A_AZZZ_) { }
// { dg-final { scan-assembler "_Z7a_AZZZ_1BIXtl2A1tlA5_cLc65EEEEE" } }

void a_AZZZZ (A_AZZZZ) { }
// { dg-final { scan-assembler "_Z7a_AZZZZ1BIXtl2A1tlA5_cLc65EEEEE" } }


typedef B<A1{ "A" }>            S_Az___;
typedef B<A1{ "A\0" }>          S_AZz__;
typedef B<A1{ "A\0\0" }>        S_AZZz_;
typedef B<A1{ "A\0\0\0" }>      S_AZZZz;

void s_Az___ (S_Az___) { }
// { dg-final { scan-assembler "_Z7s_Az___1BIXtl2A1tlA5_cLc65EEEEE" } }

void s_AZz__ (S_AZz__) { }
// { dg-final { scan-assembler "_Z7s_AZz__1BIXtl2A1tlA5_cLc65EEEEE" } }

void s_AZZz_ (S_AZZz_) { }
// { dg-final { scan-assembler "_Z7s_AZZz_1BIXtl2A1tlA5_cLc65EEEEE" } }

void s_AZZZz (S_AZZZz) { }
// { dg-final { scan-assembler "_Z7s_AZZZz1BIXtl2A1tlA5_cLc65EEEEE" } }


typedef B<A1{ 'A', 0, 0, 'D', 0 }> A_AZZDZ;
typedef B<A1{ 'A', 0, 0, 'D' }>    A_AZZD_;

void a_AZZDZ (A_AZZDZ) { }
// { dg-final { scan-assembler "_Z7a_AZZD_1BIXtl2A1tlA5_cLc65ELc0ELc0ELc68EEEEE" } }

void a_AZZD_ (A_AZZD_) { }
// { dg-final { scan-assembler "_Z7a_AZZDZ1BIXtl2A1tlA5_cLc65ELc0ELc0ELc68EEEEE" } }


typedef B<A1{ { "AB\0D" } }>  S_ABZD_;
typedef B<A1{ { "AB\0\0" } }> S_ABZZ_;
typedef B<A1{ { "AB\0" } }>   S_ABZ__;
typedef B<A1{ { "AB" } }>     S_AB___;

void s_abzd_ (S_ABZD_) { }
// { dg-final { scan-assembler "_Z7s_abzd_1BIXtl2A1tlA5_cLc65ELc66ELc0ELc68EEEEE" } }

void s_abzz_ (S_ABZZ_) { }
// { dg-final { scan-assembler "_Z7s_abzz_1BIXtl2A1tlA5_cLc65ELc66EEEEE" } }

void s_abz__ (S_ABZ__) { }
// { dg-final { scan-assembler "_Z7s_abz__1BIXtl2A1tlA5_cLc65ELc66EEEEE" } }

void s_ab___ (S_AB___) { }
// { dg-final { scan-assembler "_Z7s_ab___1BIXtl2A1tlA5_cLc65ELc66EEEEE" } }


struct A3 { char a[5], b[5], c[5]; };
template <A3> struct B3 { };

/* These all name the same type.  */
typedef B3<A3{ "\1\2",     { },             "\3\4\5\6" }> T_123z_______3456z;
typedef B3<A3{ "\1\2",     { 0 },           "\3\4\5\6" }> T_123z__Z____3456z;
typedef B3<A3{ "\1\2",     { 0, 0 },        "\3\4\5\6" }> T_123z__ZZ___3456z;
typedef B3<A3{ "\1\2",     { 0, 0, 0 },     "\3\4\5\6" }> T_123z__ZZZ__3456z;
typedef B3<A3{ "\1\2",     { 0, 0, 0, 0 },  "\3\4\5\6" }> T_123z__ZZZZ_3456z;
typedef B3<A3{ "\1\2",     "",              "\3\4\5\6" }> T_123z__Z____3456z;
typedef B3<A3{ "\1\2",     "\0",            "\3\4\5\6" }> T_123z__ZZ___3456z;
typedef B3<A3{ "\1\2",     "\0\0",          "\3\4\5\6" }> T_123z__ZZZ__3456z;
typedef B3<A3{ "\1\2",     "\0\0\0",        "\3\4\5\6" }> T_123z__ZZZZ_3456z;
typedef B3<A3{ "\1\2",     "\0\0\0\0",      "\3\4\5\6" }> T_123z__ZZZZZ3456z;
typedef B3<A3{ "\1\2\0",   "\0\0\0\0",      "\3\4\5\6" }> T_123Zz_ZZZZZ3456z;
typedef B3<A3{ "\1\2\0\0", "\0\0\0\0",      "\3\4\5\6" }> T_123ZZzZZZZZ3456z;


void ft0 (T_123z_______3456z) { }
// { dg-final { scan-assembler "_Z3ft02B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }

void ft1 (T_123z__Z____3456z) { }
// { dg-final { scan-assembler "_Z3ft12B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
void ft2 (T_123z__ZZ___3456z) { }
// { dg-final { scan-assembler "_Z3ft22B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
void ft3 (T_123z__ZZZ__3456z) { }
// { dg-final { scan-assembler "_Z3ft32B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
void ft4 (T_123z__ZZZZ_3456z) { }
// { dg-final { scan-assembler "_Z3ft42B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
void ft9 (T_123z__ZZZZZ3456z) { }
// { dg-final { scan-assembler "_Z3ft92B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
void fta (T_123Zz_ZZZZZ3456z) { }
// { dg-final { scan-assembler "_Z3fta2B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
void ftb (T_123ZZzZZZZZ3456z) { }
// { dg-final { scan-assembler "_Z3ftb2B3IXtl2A3tlA5_cLc1ELc2EEtlS1_EtlS1_Lc3ELc4ELc5ELc6EEEEE" } }
