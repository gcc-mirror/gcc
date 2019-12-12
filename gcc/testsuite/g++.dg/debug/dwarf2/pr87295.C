// { dg-additional-options "-fdebug-types-section" }
// { dg-require-effective-target c++11 }

struct A {};
namespace N {
    struct B {
	using C = struct H {};
	using D = A;
    };
}
struct E : N::B {
    typedef C C;
};
namespace N {
    struct F {
	E::C d;
	E::D h;
    };
}
struct G {
    N::F i;
} j;
