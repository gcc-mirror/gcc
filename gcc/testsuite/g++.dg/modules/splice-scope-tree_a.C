// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection" }

export module exporting_splice_scope;
// { dg-module-cmi exporting_splice_scope }

export template <typename T>
using dependent_splice_type = typename [: ^^T :];

export template <bool Cond, typename T, typename U>
using somehow_more_complicated_dependent_splice_type =
    typename [: Cond ? ^^T : ^^U :];
