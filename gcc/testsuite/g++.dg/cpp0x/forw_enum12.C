// PR c++/70513
// { dg-do compile { target c++11 } }

struct S1
{
  enum E : int;
  enum S1::E : int { X } e; // { dg-error "extra qualification not allowed" }
};

struct S2
{
  enum class E : int;
  enum class S2::E : int { X } e; // { dg-error "extra qualification not allowed" }
};

struct S3
{
  enum struct E : int;
  enum struct S3::E : int { X } e; // { dg-error "extra qualification not allowed" }
};

struct S4
{
  struct S5
  {
    enum E : char;
    enum S4::S5::E : char { X } e; // { dg-error "extra qualification not allowed" }
  };
};
