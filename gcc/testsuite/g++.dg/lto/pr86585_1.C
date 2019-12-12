struct a {
    struct b {
	b();
    } c;
};
class d {
    a e;
};
namespace aa {
    class h {};
} // namespace aa
class k {
    typedef aa::h f;
    f g;
};
namespace Inkscape {
    class l {
	k i;
class : d {
	} j;
	l();
    };
    l::l() {}
} // namespace Inkscape
