// { dg-do compile { target c++11 } }
// PR c++/79369 accept late inline of namespace

namespace X {}
inline namespace X {} // { dg-error "must be specified" }

inline namespace Y {}
namespace Y {} // OK
inline namespace Y {} // also Ok
