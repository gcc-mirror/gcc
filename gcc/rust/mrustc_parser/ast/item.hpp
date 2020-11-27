/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/item.hpp
 * - AST named item wrapper
 */
#pragma once

#include <string>
#include <vector>

namespace AST {

template <typename T>
struct Named
{
    ::std::string   name;
    T   data;
    bool    is_pub;

    Named():
        is_pub(false)
    {}
    Named(Named&&) = default;
    Named(const Named&) = default;
    Named& operator=(Named&&) = default;
    Named(::std::string name, T data, bool is_pub):
        name( ::std::move(name) ),
        data( ::std::move(data) ),
        is_pub( is_pub )
    {
    }
};

template <typename T>
using NamedList = ::std::vector<Named<T> >;

}   // namespace AST

