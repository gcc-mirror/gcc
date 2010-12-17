// { dg-do compile }
// { dg-options "-std=c++0x" }

struct S1
{
    struct S2
    {
        // opaque enum declarations
        enum class E1;
        enum class E2 : int;
        enum class E3 : short;
        enum E4 : int;
        enum E5 : short;

        // can be repeated
        enum class E1;
        enum class E2 : int;
        enum class E3 : short;
        enum E4 : int;
        enum E5 : short;
    };
};

// are complete so we can declare variables
S1::S2::E1 b1;
S1::S2::E2 b2;
S1::S2::E3 b3;
S1::S2::E4 b4;
S1::S2::E5 b5;

//even with elaborated-type-specifiers
enum S1::S2::E1 a1;
enum S1::S2::E2 a2;
enum S1::S2::E3 a3;
enum S1::S2::E4 a4;
enum S1::S2::E5 a5;

// and the list can be added later
enum class S1::S2::E1 { e11, e12 };
enum class S1::S2::E2 : int { e21, e22 };
enum class S1::S2::E3 : short {e31, e32 };
enum S1::S2::E4 : int { e41, e42 };
enum S1::S2::E5 : short { e51, e52 };

