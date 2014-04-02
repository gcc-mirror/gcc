// { dg-do compile { target c++11 } }

enum class E1 : int; // { dg-error "previous definition" }
enum E1 : int;  // { dg-error "scoped/unscoped mismatch" }

enum E2 : int; // { dg-error "previous definition" }
enum class E2 : int;  // { dg-error "scoped/unscoped mismatch" }

enum struct E3 : int;
enum class E3 : int; //ok

enum class E4 : int; // { dg-error "previous definition" }
enum class E4 : long;  // { dg-error "different underlying type" }

enum E5 : int; // { dg-error "previous definition" }
enum E5 : long;  // { dg-error "different underlying type" }

enum E6 : int;
enum E6 : int; //ok

enum class E7;
enum class E7 : int; //ok

enum class E3 e3; // { dg-error "scoped enum must not use" }
enum struct E3 e4; // { dg-error "scoped enum must not use" }
enum E5 : int e5; // { dg-error "expected|invalid type" }

enum E6 : int { a, b, c }; // { dg-error "previous definition" }
enum E6 : int { a, b, c }; // { dg-error "multiple definition" }

enum class E7 { }; // { dg-error "previous definition" }
enum class E7 { a, b, c }; // { dg-error "multiple definition" }

namespace N1
{
    struct D;
    enum class E6;
    enum E7 : int;
}

enum class N1::E6; // { dg-error "must use a simple identifier" }
enum N1::E6 e6_1; //ok
enum ::N1::E6 e6_2; //ok

namespace N2
{
    enum class N1::E6 { e1, e2, e3 }; // { dg-error "does not enclose" }
    enum N1::E7 : int { e1, e2, e3 }; // { dg-error "does not enclose" }
}

enum class N1::E6 { e1, e2, e3 };
enum N1::E7 : int { e1, e2, e3 };

struct S1
{
    struct D;
    enum class E6;
    enum E7 : int;
};

enum class S1::E6; // { dg-error "must use a simple identifier" }
enum S1::E6 e6_3; //ok
enum ::S1::E6 e6_4; //ok

struct S2
{
    enum class S1::E6 { e1, e2, e3 }; // { dg-error "does not enclose" }
    enum S1::E7 : int { e1, e2, e3 }; // { dg-error "does not enclose" }
};

enum class S1::E6 { e1, e2, e3 };
enum S1::E7 : int { e1, e2, e3 };

