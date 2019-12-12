// PR c++/61941 - Misparsing of warn_unused_result function with ref-qualifiers
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

class S
{
public:
    S x() const __attribute__ ((__warn_unused_result__));

    S y() const & __attribute__ ((__warn_unused_result__));
    S y() && __attribute__ ((__warn_unused_result__));
};
