// { dg-do assemble  }
// Origin: Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

template <template <class> class TT> class X {};
template <class T> class Y {
	X< ::Y> x;
};
