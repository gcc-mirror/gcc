// { dg-do compile }

// Origin: Ivan Godard <igodard@pacbell.net>
//	   Andrew Pinski <pinskia@gcc.gnu.org>

// PR c++/20333: ICE parsing typename without nested-name-specifier

template<class> struct f {};
f<int> f2[2] = {typename f<int>()};	// { dg-error "" }
