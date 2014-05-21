// PR c++/58664
// { dg-do compile { target c++11 } }

struct F;          // { dg-message "forward declaration" }

union U            // { dg-message "not complete" }
{
  U u;             // { dg-error "field 'u' has incomplete type 'U'" }
};

union CU           // { dg-message "not complete" }
{
  const CU u;      // { dg-error "incomplete type" }
};

template<typename T>
union UT           // { dg-message "not complete" }
{
  UT u;            // { dg-error "incomplete type" }
};

template union UT<int>;

union UF
{
  F u;             // { dg-error "field 'u' has incomplete type 'F'" }
};

template<typename T>
union UFT
{
  F u;             // { dg-error "incomplete type" }
};

template union UFT<int>;

struct S           // { dg-message "not complete" }
{
  S s;             // { dg-error "field 's' has incomplete type 'S'" }
};

struct VS          // { dg-message "not complete" }
{
  volatile VS s;   // { dg-error "incomplete type" }
};

template<typename T>
struct ST          // { dg-message "not complete" }
{
  ST s;            // { dg-error "incomplete type" }
};

template class ST<int>;

struct SF
{
  F s;             // { dg-error "field 's' has incomplete type 'F'" }
};

template<typename T>
struct SFT
{
  F s;             // { dg-error "incomplete type" }
};

template class SFT<int>;
