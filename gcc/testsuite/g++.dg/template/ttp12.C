// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Check the type of non-type parameter in template template parameter
// only if it is dependent.

template <template <int* p> class T>
struct X {};

template <typename U, template <U* p> class T>
struct Y {
    X<T> x;
};

template <int* p> struct Z {};

Y<int, Z> y1;
Y<char, Z> y2;		// { dg-error "mismatch|expected|invalid" }
