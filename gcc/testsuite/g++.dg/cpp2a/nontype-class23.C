// PR c++/90947 - Simple lookup table of array of strings is miscompiled
// Test to verify that the same specializations on non-type template
// parameters of class types are in fact treated as the same.  Unlike
// nontype-class15.C which involves only one-dimensional arrays this
// test involves arrays of arrays and arrays of structs.
// { dg-do compile { target c++2a } }

struct AA3
{
  const char a[2][2][2];
};

template <AA3> struct BAA3 { };

// Redeclare the same variable using different initialization forms
// of the same constant to verify that they are in fact all recognized
// as the same.
extern BAA3<AA3{{{ "", "" }, { "", "" }}}>       baa3;
extern BAA3<AA3{{{ "", "" }, { "", { 0, 0 } }}}> baa3;
extern BAA3<AA3{{{ "", "" }, { "", { 0 } }}}>    baa3;
extern BAA3<AA3{{{ "", "" }, { "", {} }}}>       baa3;
extern BAA3<AA3{{{ "", "" }, { "" }}}>           baa3;
extern BAA3<AA3{{{ "", "" }, { { 0, 0 } }}}>     baa3;
extern BAA3<AA3{{{ "", "" }, { { 0 } }}}>        baa3;
extern BAA3<AA3{{{ "", "" }, { {} }}}>           baa3;
extern BAA3<AA3{{{ "", "" }, { }}}>              baa3;
extern BAA3<AA3{{{ "", "" }}}>                   baa3;
extern BAA3<AA3{{{ "", { 0, 0 } }}}>             baa3;
extern BAA3<AA3{{{ "", { 0 } }}}>                baa3;
extern BAA3<AA3{{{ "", {} }}}>                   baa3;
extern BAA3<AA3{{{ "" }}}>                       baa3;
extern BAA3<AA3{{{ { 0, 0 } }}}>                 baa3;
extern BAA3<AA3{{{ { 0 } }}}>                    baa3;
extern BAA3<AA3{{{ {} }}}>                       baa3;
extern BAA3<AA3{{{ }}}>                          baa3;
extern BAA3<AA3{{ }}>                            baa3;
extern BAA3<AA3{ }>                              baa3;

extern BAA3<AA3{{{ "", "" }, { "", "1" }}}>        baa3_1;
extern BAA3<AA3{{{ "", "" }, { "", { '1', 0 } }}}> baa3_1;
extern BAA3<AA3{{{ "", "" }, { "", { '1' } }}}>    baa3_1;

extern BAA3<AA3{{{ "", "" }, { "1", {} }}}>        baa3_2;
extern BAA3<AA3{{{ "", "" }, { "1" }}}>            baa3_2;
extern BAA3<AA3{{{ "", "" }, { { '1', 0 } }}}>     baa3_2;
extern BAA3<AA3{{{ "", "" }, { { '1' } }}}>        baa3_2;

extern BAA3<AA3{{{ "", "1" }}}>                    baa3_3;
extern BAA3<AA3{{{ "", { '1', 0 } }}}>             baa3_3;
extern BAA3<AA3{{{ "", { '1' } }}}>                baa3_3;

extern BAA3<AA3{{{ "1" }}}>                        baa3_4;
extern BAA3<AA3{{{ { '1', 0 } }}}>                 baa3_4;
extern BAA3<AA3{{{ { '1' } }}}>                    baa3_4;

struct AS2
{
  struct S { const char a[2], *p; } a[2];
};

template <AS2> struct BAS2 { };

extern BAS2<AS2{{{ "", 0 }, { "", 0 }}}> bas2;
extern BAS2<AS2{{{ "", 0 }, { {}, 0 }}}> bas2;
extern BAS2<AS2{{{ "", 0 }, { "" }}}>    bas2;
extern BAS2<AS2{{{ "", 0 }, { {} }}}>    bas2;
extern BAS2<AS2{{{ "", 0 }, { }}}>       bas2;
extern BAS2<AS2{{{ "", 0 }}}>            bas2;
extern BAS2<AS2{{{ {}, 0 }}}>            bas2;
extern BAS2<AS2{{{ "" }}}>               bas2;
extern BAS2<AS2{{{ {} }}}>               bas2;
extern BAS2<AS2{{{ }}}>                  bas2;
extern BAS2<AS2{{ }}>                    bas2;
extern BAS2<AS2{ }>                      bas2;

struct AS2_2
{
  struct S { const char a[2], *p; } a[2][2];
};

template <AS2_2> struct BAS2_2 { };

extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { "", 0 }, { "", 0 }}}}> b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { "", 0 }, { "" }}}}>    b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { "", 0 }, { {} }}}}>    b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { "", 0 }, { }}}}>       b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { "", 0 } }}}>           b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { "" } }}}>              b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { {} } }}}>              b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { { }}}}>                  b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 } }, { }}}>                     b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "", 0 }}}}>                           b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { "" }}}}>                              b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { {} }}}}>                              b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }, { }}}}>                                 b2_2;
extern BAS2_2<AS2_2{{{{ "", 0 }}}}>                                      b2_2;
extern BAS2_2<AS2_2{{{{ "" }}}}>                                         b2_2;
extern BAS2_2<AS2_2{{{{ {} }}}}>                                         b2_2;
extern BAS2_2<AS2_2{{{{ }}}}>                                            b2_2;
extern BAS2_2<AS2_2{{{ }}}>                                              b2_2;
extern BAS2_2<AS2_2{{ }}>                                                b2_2;
extern BAS2_2<AS2_2{ }>                                                  b2_2;
