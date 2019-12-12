// PR c++/89833
// Test to verify that the same specializations on non-type template
// parameters of class types are in fact treated as the same.
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

// Verify the types are indeed the same by redeclaring the same identifier
// of each of them.
extern A______ same_type_B_A1;
extern A_Z____ same_type_B_A1;
extern A_ZZ___ same_type_B_A1;
extern A_ZZZ__ same_type_B_A1;
extern A_ZZZZ_ same_type_B_A1;
extern A_ZZZZZ same_type_B_A1;


// All of the following use a string to initialize the array but
// also name the same type as the above.
typedef B<A1{ "" }>                S_z____;
typedef B<A1{ "\0" }>              S_Zz___;
typedef B<A1{ "\0\0" }>            S_ZZz__;
typedef B<A1{ "\0\0\0" }>          S_ZZZz_;
typedef B<A1{ "\0\0\0\0" }>        S_ZZZZz;

// Verify the types are indeed the same by redeclaring the same identifier
// of each of them.
extern S_z____ same_type_B_A1;
extern S_Zz___ same_type_B_A1;
extern S_Zz___ same_type_B_A1;
extern S_ZZz__ same_type_B_A1;
extern S_ZZZz_ same_type_B_A1;
extern S_ZZZZz same_type_B_A1;


// All of the following also name the same type (distinct from
// the above).
typedef B<A1{ { 'A' } }>              A_A____;
typedef B<A1{ { 'A', 0 } }>           A_AZ___;
typedef B<A1{ { 'A', 0, 0 } }>        A_AZZ__;
typedef B<A1{ { 'A', 0, 0, 0 } }>     A_AZZZ_;
typedef B<A1{ { 'A', 0, 0, 0, 0 } }>  A_AZZZZ;

extern A_A____ same_type_B_A1_A;
extern A_AZ___ same_type_B_A1_A;
extern A_AZZ__ same_type_B_A1_A;
extern A_AZZZ_ same_type_B_A1_A;
extern A_AZZZZ same_type_B_A1_A;


struct A3 { char a[5], b[5], c[5]; };
template <A3> struct B3 { };

// These all name the same type.
typedef B3<A3{ }>                                   B3_A3________________;
typedef B3<A3{ { } }>                               B3_A3________________;
typedef B3<A3{ { }, { } }>                          B3_A3________________;
typedef B3<A3{ { }, { }, { } }>                     B3_A3________________;
typedef B3<A3{ { 0 }, { }, { } }>                   B3_A3________________;
typedef B3<A3{ { 0 }, { 0 }, { } }>                 B3_A3________________;
typedef B3<A3{ { 0 }, { 0 }, { 0 } }>               B3_A3________________;
typedef B3<A3{ { 0, 0 }, { 0 }, { 0 } }>            B3_A3________________;
typedef B3<A3{ { 0, 0 }, { 0, 0 }, { 0 } }>         B3_A3________________;
typedef B3<A3{ { 0, 0 }, { 0, 0 }, { 0, 0 } }>      B3_A3________________;

// These all name the same type.
typedef B3<A3{ "AB",     { },             "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     { 0 },           "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     { 0, 0 },        "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     { 0, 0, 0 },     "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     { 0, 0, 0, 0 },  "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     "",              "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     "\0",            "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     "\0\0",          "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     "\0\0\0",        "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB",     "\0\0\0\0",      "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB\0",   "\0\0\0\0",      "IJKL" }> B3_A3_AB________IJKL_;
typedef B3<A3{ "AB\0\0", "\0\0\0\0",      "IJKL" }> B3_A3_AB________IJKL_;

// Types with the same name must be the same (and so redefinitions
// must be accepted).  Likewise, overloads on distinct types must
// be accepted.
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {0,0,0,0,0} }> B3_A3________________;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {0} }>         B3_A3________________;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {} }>          B3_A3________________;
typedef B3<A3{ {0,0,0,0,0}, {0}, {} }>                  B3_A3________________;
typedef B3<A3{ {0,0,0,0,0}, {}, {} }>                   B3_A3________________;
typedef B3<A3{ {0}, {0}, {0} }>                         B3_A3________________;
typedef B3<A3{ {0}, {0}, {} }>                          B3_A3________________;
typedef B3<A3{ {0}, {}, {0} }>                          B3_A3________________;
typedef B3<A3{ {}, {}, {} }>                            B3_A3________________;
typedef B3<A3{ {}, {} }>                                B3_A3________________;
typedef B3<A3{ {} }>                                    B3_A3________________;
typedef B3<A3{ }>                                       B3_A3________________;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0},   {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0},     {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0,0,0}, {0,0},       {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0,0,0}, {0},         {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0,0,0}, {},          {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0,0},   {0,0,0,0,0}, {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0},     {0,0,0,0},   {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0},       {0,0,0},     {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0},         {0,0,},      {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {},          {0},         {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {},          {},          {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ {0,0,0},     {0,0},       {0,0,0,1,0} }> B3_A3______________1_;
typedef B3<A3{ {0,0,0},     {0,0},       {0,0,0,1} }>   B3_A3______________1_;
typedef B3<A3{ {0},         {},          {0,0,1,0} }>   B3_A3_____________1__;
typedef B3<A3{ {0},         {},          {0,0,1} }>     B3_A3_____________1__;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {0,1,0} }>     B3_A3____________1___;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {0,1} }>       B3_A3____________1___;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,0}, {1} }>         B3_A3___________1____;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,1}, {0,0,0,0,0} }> B3_A3__________1_____;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,1}, {} }>          B3_A3__________1_____;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,0,1} }>              B3_A3__________1_____;
typedef B3<A3{ {0,0,0,0,0}, {0,0,0,1,0}, {0,0,0,0,0} }> B3_A3_________1______;
typedef B3<A3{ {0,0,0,0,0}, {0,0,1,0,0}, {0,0,0,0,0} }> B3_A3________1_______;
typedef B3<A3{ {0,0,0,0,0}, {0,1,0,0,0}, {0,0,0,0,0} }> B3_A3_______1________;
typedef B3<A3{ {0,0,0,0,0}, {1,0,0,0,0}, {0,0,0,0,0} }> B3_A3______1_________;
typedef B3<A3{ {0,0,0,0,1}, {0,0,0,0,0}, {0,0,0,0,0} }> B3_A3_____1__________;
typedef B3<A3{ {0,0,0,0,1}, {0,0,0,0,0} }>              B3_A3_____1__________;
typedef B3<A3{ {0,0,0,0,1} }>                           B3_A3_____1__________;
typedef B3<A3{ {0,0,0,1,0}, {0,0,0,0,0}, {0,0,0,0,0} }> B3_A3____1___________;
typedef B3<A3{ {0,0,1,0,0}, {0,0,0,0,0}, {0,0,0,0,0} }> B3_A3___1____________;
typedef B3<A3{ {0,1,0,0,0}, {0,0,0,0,0}, {0,0,0,0,0} }> B3_A3__1_____________;
typedef B3<A3{ {1,0,0,0,0}, {0,0,0,0,0}, {0,0,0,0,0} }> B3_A3_1______________;
typedef B3<A3{ {1,0,0,0,0}, {0,0,0,0,0} }>              B3_A3_1______________;
typedef B3<A3{ {1,0,0,0,0} }>                           B3_A3_1______________;
typedef B3<A3{ {1,0,0,0} }>                             B3_A3_1______________;
typedef B3<A3{ {1,0,0} }>                               B3_A3_1______________;
typedef B3<A3{ {1,0} }>                                 B3_A3_1______________;
typedef B3<A3{ {1} }>                                   B3_A3_1______________;

typedef B3<A3{ {1,0,0,0,0}, {0,0,0,0,0}, {0,0,0,0,1} }> B3_A3_1_____________1;
typedef B3<A3{ {1},         {0},         {0,0,0,0,1} }> B3_A3_1_____________1;
typedef B3<A3{ {1},         {},          {0,0,0,0,1} }> B3_A3_1_____________1;
typedef B3<A3{ {0,1,0,0,0}, {0,0,0,0,0}, {0,0,0,1} }>   B3_A3__1___________1_;
typedef B3<A3{ {0,1},       {0},         {0,0,0,1} }>   B3_A3__1___________1_;
typedef B3<A3{ {0,1},       {},          {0,0,0,1} }>   B3_A3__1___________1_;

// Same as above.
typedef B3<A3{ "\0\0\0\0",  "\0\0\0\0",   "\0\0\0\0" }> B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0\0",   "\0\0\0" }>   B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0\0",   "\0\0" }>     B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0\0",   "\0" }>       B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0\0",   "" }>         B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0",     "" }>         B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0",       "" }>         B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0",         "" }>         B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "",           "" }>         B3_A3________________;
typedef B3<A3{ "\0\0\0",    "",           "" }>         B3_A3________________;
typedef B3<A3{ "\0\0",      "",           "" }>         B3_A3________________;
typedef B3<A3{ "\0",        "",           "" }>         B3_A3________________;
typedef B3<A3{ "",          "",           "" }>         B3_A3________________;
typedef B3<A3{ "",          "" }>                       B3_A3________________;
typedef B3<A3{ "" }>                                    B3_A3________________;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0",     { 0 } }>      B3_A3________________;
typedef B3<A3{ "\0\0",      { 0 },        "\0" }>       B3_A3________________;
typedef B3<A3{ { 0 },       "",           "\0\0\0\0" }> B3_A3________________;
typedef B3<A3{ "\0\0\0",    "\0\0",       { } }>        B3_A3________________;
typedef B3<A3{ "\0",        { },          "" }>         B3_A3________________;
typedef B3<A3{ { },         "\0\0\0\0",   "\0\0\0" }>   B3_A3________________;

typedef B3<A3{ "\0\0\0\0",  "\0\0\0",    {0,0,0,0,1} }> B3_A3_______________1;
typedef B3<A3{ "\0\0",      "\0",        "\0\0\0\1" }>  B3_A3______________1_;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0",    "\0\0\1" }>    B3_A3_____________1__;
typedef B3<A3{ "\0\0",      "\0",        "\0\1" }>      B3_A3____________1___;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0",    "\1" }>        B3_A3___________1____;
typedef B3<A3{ "\0\0",      {0,0,0,0,1}, "\0" }>        B3_A3__________1_____;
typedef B3<A3{ "\0\0\0\0",  "\0\0\0\1",  "\0\0\0" }>    B3_A3_________1______;
typedef B3<A3{ "\0\0",      "\0\0\1",    "\0" }>        B3_A3________1_______;
typedef B3<A3{ "\0\0\0\0",  "\0\1",      "\0\0\0" }>    B3_A3_______1________;
typedef B3<A3{ "\0\0",      "\1",        "\0" }>        B3_A3______1_________;
typedef B3<A3{ {0,0,0,0,1}, "\0\0\0\0",  "\0\0\0" }>    B3_A3_____1__________;
typedef B3<A3{ "\0\0\0\1",  "\0\0",      "\0" }>        B3_A3____1___________;
typedef B3<A3{ "\0\0\1",    "\0\0\0\0",  "\0\0\0" }>    B3_A3___1____________;
typedef B3<A3{ "\0\1",      "\0\0",      "\0" }>        B3_A3__1_____________;
typedef B3<A3{ "\1",        "",          "\0\0\0\0" }>  B3_A3_1______________;

typedef B3<A3{ "\1",        {},          {0,0,0,0,1} }> B3_A3_1_____________1;
typedef B3<A3{ "\1",        "",          {0,0,0,0,1} }> B3_A3_1_____________1;
typedef B3<A3{ "\0\1",      {},          {0,0,0,1} }>   B3_A3__1___________1_;
typedef B3<A3{ "\0\1",      "",          "\0\0\0\1" }>  B3_A3__1___________1_;
typedef B3<A3{ "\0\1\0",    "\0",        "\0\0\0\1" }>  B3_A3__1___________1_;

void f_b3_a3 (B3_A3________________) { }
void f_b3_a3 (B3_A3_______________1) { }
void f_b3_a3 (B3_A3______________1_) { }
void f_b3_a3 (B3_A3_____________1__) { }
void f_b3_a3 (B3_A3____________1___) { }
void f_b3_a3 (B3_A3___________1____) { }
void f_b3_a3 (B3_A3__________1_____) { }
void f_b3_a3 (B3_A3_________1______) { }
void f_b3_a3 (B3_A3________1_______) { }
void f_b3_a3 (B3_A3_______1________) { }
void f_b3_a3 (B3_A3______1_________) { }
void f_b3_a3 (B3_A3_____1__________) { }
void f_b3_a3 (B3_A3____1___________) { }
void f_b3_a3 (B3_A3___1____________) { }
void f_b3_a3 (B3_A3__1_____________) { }
void f_b3_a3 (B3_A3_1______________) { }
void f_b3_a3 (B3_A3_1_____________1) { }
void f_b3_a3 (B3_A3__1___________1_) { }

typedef B3<A3{ "AB\0D",  { },             "IJKL" }> B3_A3_ABZDZZZZZZIJKLZ;
typedef B3<A3{ "AB\0D",  { 0, 0, 1 },     "IJKL" }> B3_A3_ABZDZZZ1ZZIJKLZ;
typedef B3<A3{ "AB\0D",  { 0, 1 },        "IJKL" }> B3_A3_ABZDZZ1ZZZIJKLZ;

void f (B3_A3_ABZDZZZZZZIJKLZ) { }
void f (B3_A3_ABZDZZZ1ZZIJKLZ) { }
void f (B3_A3_ABZDZZ1ZZZIJKLZ) { }
