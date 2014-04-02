// { dg-do compile { target c++11 } }

namespace one
{
    struct S
    {
        enum { A = 1, B = 2 };
        struct T
        {
            enum { B = 102 };

            enum class E1;
            enum E2 : int;
        };
    };

    enum class S::T::E1 { A1 = A, B1 = B, C1 };
    enum S::T::E2 : int { A1 = A, B1 = B, C1 };

    static_assert(int(S::T::E1::A1) == 1, "error");
    static_assert(int(S::T::E1::B1) == 102, "error");
    static_assert(int(S::T::E1::C1) == 103, "error");

    static_assert(int(S::T::E2::A1) == 1, "error");
    static_assert(int(S::T::E2::B1) == 102, "error");
    static_assert(int(S::T::E2::C1) == 103, "error");
    static_assert(int(S::T::A1) == 1, "error");
    static_assert(int(S::T::B1) == 102, "error");
    static_assert(int(S::T::C1) == 103, "error");
}


namespace two
{
    namespace S
    {
        enum { A = 1, B = 2 };
        namespace T
        {
            enum { B = 102 };

            enum class E1;
            enum E2 : int;
        }
    }

    enum class S::T::E1 { A1 = A, B1 = B, C1 };
    enum S::T::E2 : int { A1 = A, B1 = B, C1 };

    static_assert(int(S::T::E1::A1) == 1, "error");
    static_assert(int(S::T::E1::B1) == 102, "error");
    static_assert(int(S::T::E1::C1) == 103, "error");

    static_assert(int(S::T::E2::A1) == 1, "error");
    static_assert(int(S::T::E2::B1) == 102, "error");
    static_assert(int(S::T::E2::C1) == 103, "error");
    static_assert(int(S::T::A1) == 1, "error");
    static_assert(int(S::T::B1) == 102, "error");
    static_assert(int(S::T::C1) == 103, "error");
}


