// PR c++/58664
// { dg-do compile { target c++11 } }

struct F;          // { dg-message "forward declaration" }

union U            // { dg-message "not complete" }
{
  U u[1] = { 0 };  // { dg-error "incomplete type" }
};

template<typename T>
union UT           // { dg-message "not complete" }
{
  UT u[1] = { 0 }; // { dg-error "incomplete type" }
};

template union UT<int>;

union UF
{
  F u[1] = { 0 };  // { dg-error "incomplete type" }
};

template<typename T>
union UFT
{
  F u[1] = { 0 };  // { dg-error "incomplete type" }
};

template union UFT<int>;

struct S           // { dg-message "not complete" }
{
  S s[1] = { 0 };  // { dg-error "incomplete type" }
};

template<typename T>
struct ST          // { dg-message "not complete" }
{
  ST s[1] = { 0 }; // { dg-error "incomplete type" }
};

template class ST<int>;

struct SF
{
  F s[1] = { 0 };  // { dg-error "incomplete type" }
};

template<typename T>
struct SFT
{
  F s[1] = { 0 };  // { dg-error "incomplete type" }
};

template class SFT<int>;
