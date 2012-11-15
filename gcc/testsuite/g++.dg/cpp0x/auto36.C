// PR c++/54903
// { dg-options -std=c++11 }

template<int N, int D>
struct Modulus
{
        static auto const value = N % D;
};

template<int N>
struct Angle
{
        static auto const value = Modulus<N, 360>::value; // ERROR
        //static int const value = Modulus<N, 360>::value;  // OK
        //static auto const value = N % 360;                // OK

        typedef Angle<value> type;
};
