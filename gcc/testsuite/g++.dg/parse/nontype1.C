// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// PR c++/13289: Incorrectly reject non-type template argument that has
// dependent type

template <class T, T t> class C {};
template <class T, T t> class D { C<T, t-1> c; };
