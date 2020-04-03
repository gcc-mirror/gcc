// { dg-do compile { target c++2a } }

namespace a
{
  template<typename = int>
    concept b = true; // { dg-message ".a::b. declared here" }
}

static_assert(b); // { dg-error "did you mean .a::b." }

namespace c
{
  template<typename>
    concept b = true; // { dg-message "concept c::b." }

  template<typename>
    concept b = true; // { dg-error "concept c::b." }
}
