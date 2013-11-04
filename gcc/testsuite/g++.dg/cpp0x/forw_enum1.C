// { dg-do compile }
// { dg-options "-std=c++11" }

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

// are complete so we can declare variables
E1 b1;
E2 b2;
E3 b3;
E4 b4;
E5 b5;

//even with elaborated-type-specifiers
enum E1 a1;
enum E2 a2;
enum E3 a3;
enum E4 a4;
enum E5 a5;

// and the list can be added later
enum class E1 { e11, e12 };
enum class E2 : int { e21, e22 };
enum class E3 : short {e31, e32 };
enum E4 : int { e41, e42 };
enum E5 : short { e51, e52 };

// more repetitions allowed
enum class E1;
enum class E2 : int;
enum class E3 : short;
enum E4 : int;
enum E5 : short;

