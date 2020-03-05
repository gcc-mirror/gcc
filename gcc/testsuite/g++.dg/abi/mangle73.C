// { dg-do compile { target c++2a } }

struct A
{
  char a[2][2];
};

template <A> struct B { };

typedef B<A{ { { 0, 0 }, { 0, 0 } } }> AZZZZ;
typedef B<A{ { { 0, 0 }, { 0 } } }>    AZZZ_;
typedef B<A{ { { 0, 0 } } }>           AZZ__;
typedef B<A{ { { 0 } } }>              AZ___;
typedef B<A{ { { } } }>                A____;

typedef B<A{ { { "" }, { "" } } }>     AS_S_;
typedef B<A{ { { "" }, { 0, 0 } } }>   AS_ZZ;
typedef B<A{ { { "" }, { 0 } } }>      AS_Z_;
typedef B<A{ { { "" } } }>             AS___;


// Verify that the types mangle the same.
void a_zzzz (AZZZZ) { }
// { dg-final { scan-assembler "_Z6a_zzzz1BIXtl1AEEE" } }

void a_zzz_ (AZZZ_) { }
// { dg-final { scan-assembler "_Z6a_zzz_1BIXtl1AEEE" } }

void a_zz__ (AZZ__) { }
// { dg-final { scan-assembler "_Z6a_zz__1BIXtl1AEEE" } }

void a_z___ (AZ___) { }
// { dg-final { scan-assembler "_Z6a_z___1BIXtl1AEEE" } }

void a_____ (A____) { }
// { dg-final { scan-assembler "_Z6a_____1BIXtl1AEEE" } }

void a_s_s_ (AS_S_) { }
// { dg-final { scan-assembler "_Z6a_s_s_1BIXtl1AEEE" } }

void a_s_zz (AS_ZZ) { }
// { dg-final { scan-assembler "_Z6a_s_zz1BIXtl1AEEE" } }

void a_s_z_ (AS_Z_) { }
// { dg-final { scan-assembler "_Z6a_s_z_1BIXtl1AEEE" } }

void a_s___ (AS___) { }
// { dg-final { scan-assembler "_Z6a_s___1BIXtl1AEEE" } }


struct C
{
  struct { const char a[2][2], *p; } a[2];
};

template <C> struct D { };

typedef D<C{{{{{ 0, 0 }, { 0, 0 }}, 0 }, {{{ 0, 0 }, { 0, 0 }}, 0 }}}> DZZZZZZZZZZ;
typedef D<C{{{{{ 0, 0 }, { 0, 0 }}, 0 }, {{{ 0, 0 }, { 0, 0 }}}}}> DZZZZZZZZZ_;
typedef D<C{{{{{ 0, 0 }, { 0, 0 }}, 0 }, {{{ 0, 0 }, { 0 }}}}}>    DZZZZZZZZ__;
typedef D<C{{{{{ 0, 0 }, { 0, 0 }}, 0 }, {{{ 0, 0 } }}}}>          DZZZZZZZ___;
typedef D<C{{{{{ 0, 0 }, { 0, 0 }}, 0 }, {{{ 0 } }}}}>             DZZZZZZ____;
typedef D<C{{{{{ 0, 0 }, { 0, 0 }}, 0 }}}>                         DZZZZZ_____;
typedef D<C{{{{{ 0, 0 }, { 0, 0 }}}}}>                             DZZZZ______;
typedef D<C{{{{{ 0, 0 }, { 0 }}}}}>                                DZZZ_______;
typedef D<C{{{{{ 0, 0 }}}}}>                                       DZZ________;
typedef D<C{{{{{ 0 }}}}}>                                          DZ_________;
typedef D<C{ }>                                                    D__________;

typedef D<C{{{{{ "" }, { "" }}, 0 }, {{{ "" }, { "" }}, 0 }}}>     DS_S_ZS_S_Z;

void d_zzzzzzzzzz (DZZZZZZZZZZ) { }
// { dg-final { scan-assembler "_Z12d_zzzzzzzzzz1DIXtl1CEEE" } }
void d_zzzzzzzzz_ (DZZZZZZZZZ_) { }
// { dg-final { scan-assembler "_Z12d_zzzzzzzzz_1DIXtl1CEEE" } }
void d_zzzzzzzz__ (DZZZZZZZZ__) { }
// { dg-final { scan-assembler "_Z12d_zzzzzzzz__1DIXtl1CEEE" } }
void d_zzzzzzz___ (DZZZZZZZ___) { }
// { dg-final { scan-assembler "_Z12d_zzzzzzz___1DIXtl1CEEE" } }
void d_zzzzzz____ (DZZZZZZ____) { }
// { dg-final { scan-assembler "_Z12d_zzzzzz____1DIXtl1CEEE" } }
void d_zzzzz_____ (DZZZZZ_____) { }
// { dg-final { scan-assembler "_Z12d_zzzzz_____1DIXtl1CEEE" } }
void d_zzzz______ (DZZZZ______) { }
// { dg-final { scan-assembler "_Z12d_zzzz______1DIXtl1CEEE" } }
void d_zzz_______ (DZZZ_______) { }
// { dg-final { scan-assembler "_Z12d_zzz_______1DIXtl1CEEE" } }
void d_zz________ (DZZ________) { }
// { dg-final { scan-assembler "_Z12d_zz________1DIXtl1CEEE" } }
void d_z_________ (DZ_________) { }
// { dg-final { scan-assembler "_Z12d_z_________1DIXtl1CEEE" } }
void d___________ (D__________) { }
// { dg-final { scan-assembler "_Z12d___________1DIXtl1CEEE" } }

void d_s_s_zs_s_z (DS_S_ZS_S_Z) { }
// { dg-final { scan-assembler "_Z12d_s_s_zs_s_z1DIXtl1CEEE" } }
