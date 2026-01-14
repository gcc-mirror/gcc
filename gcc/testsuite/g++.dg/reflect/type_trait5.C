// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], type properties.

#include <meta>
using namespace std::meta;

struct C { };

struct HasTemplateCCtor
{
  HasTemplateCCtor (const HasTemplateCCtor &) = default;
  template <class T>
  HasTemplateCCtor (T &&);
};
                
struct MoveOnly
{
  MoveOnly (MoveOnly &&) = default;
};
  
struct MoveOnly2
{
  MoveOnly2 (MoveOnly2 &&) = delete;
};

class EmptyClassOne
{ typedef int type; };

class EmptyClassTwo
{ static int data; };

class EmptyClassThree
{ int f(); };

class NonEmptyClassOne
{ int data; };

class NonEmptyClassTwo
{
  virtual int f();
  virtual ~NonEmptyClassTwo();
};

class ClassType { };
typedef const ClassType cClassType;
typedef volatile ClassType vClassType;
typedef const volatile ClassType cvClassType;

class DerivedType : public ClassType { };

class FinalType final : public DerivedType { };

enum EnumType { e0 };

struct ConvType
{ operator int () const; };

class AbstractClass
{
  virtual void rotate (int) = 0;
};

class PolymorphicClass
{
  virtual void rotate (int);
};

class DerivedPolymorphic : public PolymorphicClass { };

class VirtualDestructorClass
{
  virtual ~VirtualDestructorClass();
};

union UnionType { };

union IncompleteUnion;

class IncompleteClass;

struct ExplicitClass
{
  ExplicitClass (double &);
  explicit ExplicitClass (int &);
  ExplicitClass (double &, int &, double &);
};

struct NoexceptMoveAssignClass
{
  NoexceptMoveAssignClass (NoexceptMoveAssignClass &&) = default;
  NoexceptMoveAssignClass &operator= (NoexceptMoveAssignClass &&) noexcept (true);
};

struct NType
{
  int i;
  int j;
  virtual ~NType ();
};

struct TType
{
  int i;
private:
  int j;
};

struct SLType
{
  int i;
  int j;
  ~SLType ();
};

struct PODType
{
  int i;
  int j;
};

struct CopyConsOnlyType
{
  CopyConsOnlyType (int) { }
  CopyConsOnlyType (CopyConsOnlyType &&) = delete;
  CopyConsOnlyType (const CopyConsOnlyType &) = default;
  CopyConsOnlyType &operator= (const CopyConsOnlyType &) = delete;
  CopyConsOnlyType &operator= (CopyConsOnlyType &&) = delete;
};

struct MoveConsOnlyType
{
  MoveConsOnlyType (int) { }
  MoveConsOnlyType (const MoveConsOnlyType &) = delete;
  MoveConsOnlyType (MoveConsOnlyType &&) = default;
  MoveConsOnlyType& operator= (const MoveConsOnlyType &) = delete;
  MoveConsOnlyType& operator= (MoveConsOnlyType &&) = delete;
};

struct Incomplete_struct;

namespace construct
{
  struct Empty {};

  struct B { int i; B () {} };

  union U { int i; Empty b; };

  struct Abstract
  {
    virtual ~Abstract () = 0;
  };

  struct Any
  {
    template <class T>
    Any (T &&) {}
  };

  struct DelDef
  {
    DelDef () = delete;
  };

  struct DelCopy
  {
    DelCopy (const DelCopy &) = delete;
  };

  struct DelDtor
  {
    DelDtor () = default;
    DelDtor (const DelDtor &) = default;
    DelDtor (DelDtor &&) = default;
    DelDtor (int);
    DelDtor (int, B, U);
    ~DelDtor () = delete;
  };

  struct Ellipsis
  {
    Ellipsis (...) {}
  };

  struct DelEllipsis
  {
    DelEllipsis (...) = delete;
  };

  struct Nontrivial
  {
    Nontrivial ();
    Nontrivial (const Nontrivial &);
    Nontrivial &operator= (const Nontrivial &);
    ~Nontrivial ();
  };

  struct UnusualCopy
  {
    UnusualCopy (UnusualCopy &);
  };
}

namespace N
{
  void foo ();
}

int v = 1;
struct S1 { decltype (^^long) a; };
union U2 { int a; decltype (^^N::foo) b; };
struct S3 { const decltype (^^N) *c; };
struct S4 : public S3 {};
struct S5 { int a; long *b; };

static_assert (is_const_type (^^const int));
static_assert (is_const_type (^^const volatile int));
static_assert (is_const_type (^^cClassType));
static_assert (is_const_type (^^cvClassType));
static_assert (!is_const_type (^^int));
static_assert (!is_const_type (^^volatile int));
static_assert (!is_const_type (^^ClassType));
static_assert (!is_const_type (^^vClassType));

static_assert (is_volatile_type (^^volatile int));
static_assert (is_volatile_type (^^const volatile int));
static_assert (is_volatile_type (^^vClassType));
static_assert (is_volatile_type (^^cvClassType));
static_assert (!is_volatile_type (^^int));
static_assert (!is_volatile_type (^^const int));
static_assert (!is_volatile_type (^^ClassType));
static_assert (!is_volatile_type (^^cClassType));

static_assert (is_trivially_copyable_type (^^int));
static_assert (is_trivially_copyable_type (^^volatile int));
static_assert (is_trivially_copyable_type (^^TType));
static_assert (is_trivially_copyable_type (^^PODType));
static_assert (!is_trivially_copyable_type (^^NType));
static_assert (!is_trivially_copyable_type (^^SLType));
static_assert (is_trivially_copyable_type (^^construct::DelDef));
static_assert (!is_trivially_copyable_type (^^construct::Abstract));
static_assert (is_trivially_copyable_type (^^construct::Ellipsis));
static_assert (is_trivially_copyable_type (^^construct::DelEllipsis));
static_assert (is_trivially_copyable_type (^^construct::Any));
static_assert (is_trivially_copyable_type (^^construct::DelCopy));
static_assert (is_trivially_copyable_type (^^construct::DelDtor));
static_assert (!is_trivially_copyable_type (^^construct::Nontrivial));
static_assert (!is_trivially_copyable_type (^^construct::UnusualCopy));
static_assert (is_trivially_copyable_type (^^CopyConsOnlyType));
static_assert (is_trivially_copyable_type (^^MoveConsOnlyType));
static_assert (is_trivially_copyable_type (^^HasTemplateCCtor));
static_assert (is_trivially_copyable_type (^^MoveOnly));
static_assert (is_trivially_copyable_type (^^MoveOnly2));
static_assert (is_trivially_copyable_type (^^volatile int));
static_assert (is_trivially_copyable_type (^^TType));
static_assert (is_trivially_copyable_type (^^PODType));
static_assert (!is_trivially_copyable_type (^^NType));
static_assert (!is_trivially_copyable_type (^^SLType));
static_assert (is_trivially_copyable_type (^^construct::DelDef));
static_assert (!is_trivially_copyable_type (^^construct::Abstract));
static_assert (is_trivially_copyable_type (^^construct::Ellipsis));
static_assert (is_trivially_copyable_type (^^construct::DelEllipsis));
static_assert (is_trivially_copyable_type (^^construct::Any));
static_assert (is_trivially_copyable_type (^^construct::DelCopy));
static_assert (is_trivially_copyable_type (^^construct::DelDtor));
static_assert (!is_trivially_copyable_type (^^construct::Nontrivial));
static_assert (!is_trivially_copyable_type (^^construct::UnusualCopy));
static_assert (is_trivially_copyable_type (^^CopyConsOnlyType));
static_assert (is_trivially_copyable_type (^^MoveConsOnlyType));
static_assert (is_trivially_copyable_type (^^HasTemplateCCtor));
static_assert (is_trivially_copyable_type (^^MoveOnly));
static_assert (is_trivially_copyable_type (^^MoveOnly2));

static_assert (is_standard_layout_type (^^SLType));
static_assert (is_standard_layout_type (^^PODType));
static_assert (!is_standard_layout_type (^^NType));
static_assert (!is_standard_layout_type (^^TType));

static_assert (is_empty_type (^^ClassType));
static_assert (is_empty_type (^^EmptyClassOne));
static_assert (is_empty_type (^^EmptyClassTwo));
static_assert (is_empty_type (^^EmptyClassThree));
static_assert (!is_empty_type (^^void));
static_assert (!is_empty_type (^^float));
static_assert (!is_empty_type (^^int[4]));
static_assert (!is_empty_type (^^int *));
static_assert (!is_empty_type (^^int &));
static_assert (!is_empty_type (^^int (ClassType::*)));
static_assert (!is_empty_type (^^EnumType));
static_assert (!is_empty_type (^^int (int)));
static_assert (!is_empty_type (^^AbstractClass));
static_assert (!is_empty_type (^^NonEmptyClassOne));
static_assert (!is_empty_type (^^NonEmptyClassTwo));

static_assert (is_polymorphic_type (^^PolymorphicClass));
static_assert (is_polymorphic_type (^^DerivedPolymorphic));
static_assert (is_polymorphic_type (^^AbstractClass));
static_assert (is_polymorphic_type (^^VirtualDestructorClass));
static_assert (!is_polymorphic_type (^^void));
static_assert (!is_polymorphic_type (^^int (int)));
static_assert (!is_polymorphic_type (^^int &));
static_assert (!is_polymorphic_type (^^EnumType));
static_assert (!is_polymorphic_type (^^ClassType));
static_assert (!is_polymorphic_type (^^DerivedType));

static_assert (is_abstract_type (^^AbstractClass));
static_assert (!is_abstract_type (^^void));
static_assert (!is_abstract_type (^^int (int)));
static_assert (!is_abstract_type (^^int &));
static_assert (!is_abstract_type (^^ClassType));

static_assert (is_final_type (^^FinalType));
static_assert (!is_final_type (^^ClassType));
static_assert (!is_final_type (^^DerivedType));

static_assert (is_aggregate_type (^^ClassType));
static_assert (is_aggregate_type (^^UnionType));
static_assert (is_aggregate_type (^^SLType));
static_assert (is_aggregate_type (^^unsigned[3]));
static_assert (is_aggregate_type (^^unsigned[3][2]));
static_assert (is_aggregate_type (^^unsigned[]));
static_assert (is_aggregate_type (^^unsigned[][2]));
static_assert (is_aggregate_type (^^EnumType[3]));
static_assert (is_aggregate_type (^^EnumType[3][2]));
static_assert (is_aggregate_type (^^EnumType[]));
static_assert (is_aggregate_type (^^EnumType[][2]));
static_assert (!is_aggregate_type (^^AbstractClass));
static_assert (!is_aggregate_type (^^PolymorphicClass));
static_assert (!is_aggregate_type (^^ExplicitClass));
static_assert (!is_aggregate_type (^^char));
static_assert (!is_aggregate_type (^^unsigned char));
static_assert (!is_aggregate_type (^^signed char));
static_assert (!is_aggregate_type (^^unsigned));
static_assert (!is_aggregate_type (^^bool));
static_assert (!is_aggregate_type (^^float));
static_assert (!is_aggregate_type (^^double));
static_assert (!is_aggregate_type (^^EnumType));
static_assert (!is_aggregate_type (^^void));
static_assert (!is_aggregate_type (^^NoexceptMoveAssignClass));

static_assert (is_consteval_only_type (^^decltype (^^long)));
static_assert (is_consteval_only_type (^^const decltype (^^N::foo)));
static_assert (is_consteval_only_type (^^volatile decltype (^^N)));
static_assert (is_consteval_only_type (^^const volatile decltype (^^v)));
static_assert (is_consteval_only_type (^^const S1));
static_assert (is_consteval_only_type (^^U2));
static_assert (is_consteval_only_type (^^S3));
static_assert (is_consteval_only_type (^^S4));
static_assert (!is_consteval_only_type (^^int));
static_assert (!is_consteval_only_type (^^S5));

static_assert (!is_signed_type (^^void));
static_assert (char (-1) < char (0) ? is_signed_type (^^char) : !is_signed_type (^^char));
static_assert (is_signed_type (^^signed char));
static_assert (!is_signed_type (^^unsigned char));
static_assert (wchar_t (-1) < wchar_t (0) ? is_signed_type (^^wchar_t) : !is_signed_type (^^wchar_t));
static_assert (is_signed_type (^^short));
static_assert (!is_signed_type (^^unsigned short));
static_assert (is_signed_type (^^int));
static_assert (!is_signed_type (^^unsigned int));
static_assert (is_signed_type (^^long));
static_assert (!is_signed_type (^^unsigned long));
static_assert (is_signed_type (^^long long));
static_assert (!is_signed_type (^^unsigned long long));
static_assert (is_signed_type (^^float));
static_assert (is_signed_type (^^double));
static_assert (is_signed_type (^^long double));
static_assert (!is_signed_type (^^ClassType));

static_assert (!is_unsigned_type (^^void));
static_assert (char (-1) < char (0) ? !is_unsigned_type (^^char) : is_unsigned_type (^^char));
static_assert (!is_unsigned_type (^^signed char));
static_assert (is_unsigned_type (^^unsigned char));
static_assert (wchar_t (-1) < wchar_t (0) ? !is_unsigned_type (^^wchar_t) : is_unsigned_type (^^wchar_t));
static_assert (!is_unsigned_type (^^short));
static_assert (is_unsigned_type (^^unsigned short));
static_assert (!is_unsigned_type (^^int));
static_assert (is_unsigned_type (^^unsigned int));
static_assert (!is_unsigned_type (^^long));
static_assert (is_unsigned_type (^^unsigned long));
static_assert (!is_unsigned_type (^^long long));
static_assert (is_unsigned_type (^^unsigned long long));
static_assert (!is_unsigned_type (^^float));
static_assert (!is_unsigned_type (^^double));
static_assert (!is_unsigned_type (^^long double));
static_assert (!is_unsigned_type (^^ClassType));

static_assert (is_bounded_array_type (^^int[2]));
static_assert (!is_bounded_array_type (^^int[]));
static_assert (is_bounded_array_type (^^int[2][3]));
static_assert (!is_bounded_array_type (^^int[][3]));
static_assert (is_bounded_array_type (^^float *[2]));
static_assert (!is_bounded_array_type (^^float *[]));
static_assert (is_bounded_array_type (^^float *[2][3]));
static_assert (!is_bounded_array_type (^^float *[][3]));
static_assert (is_bounded_array_type (^^ClassType[2]));
static_assert (!is_bounded_array_type (^^ClassType[]));
static_assert (is_bounded_array_type (^^ClassType[2][3]));
static_assert (!is_bounded_array_type (^^ClassType[][3]));
static_assert (!is_bounded_array_type (^^int (*)[2]));
static_assert (!is_bounded_array_type (^^int (*)[]));
static_assert (!is_bounded_array_type (^^int (&)[2]));
static_assert (!is_bounded_array_type (^^int (&)[]));
static_assert (!is_bounded_array_type (^^ClassType));
static_assert (!is_bounded_array_type (^^void ()));

static_assert (!is_unbounded_array_type (^^int[2]));
static_assert (is_unbounded_array_type (^^int[]));
static_assert (!is_unbounded_array_type (^^int[2][3]));
static_assert (is_unbounded_array_type (^^int[][3]));
static_assert (!is_unbounded_array_type (^^float *[2]));
static_assert (is_unbounded_array_type (^^float *[]));
static_assert (!is_unbounded_array_type (^^float *[2][3]));
static_assert (is_unbounded_array_type (^^float *[][3]));
static_assert (!is_unbounded_array_type (^^ClassType[2]));
static_assert (is_unbounded_array_type (^^ClassType[]));
static_assert (!is_unbounded_array_type (^^ClassType[2][3]));
static_assert (is_unbounded_array_type (^^ClassType[][3]));
static_assert (!is_unbounded_array_type (^^IncompleteClass[2][3]));
static_assert (is_unbounded_array_type (^^IncompleteClass[][3]));
static_assert (!is_unbounded_array_type (^^int (*)[2]));
static_assert (!is_unbounded_array_type (^^int (*)[]));
static_assert (!is_unbounded_array_type (^^int (&)[2]));
static_assert (!is_unbounded_array_type (^^int (&)[]));
static_assert (!is_unbounded_array_type (^^ClassType));
static_assert (!is_unbounded_array_type (^^IncompleteClass));
static_assert (!is_unbounded_array_type (^^IncompleteUnion));

enum class E { e1, e2 };
static_assert (is_scoped_enum_type (^^E));
enum class Ec : char { e1, e2 };
static_assert (is_scoped_enum_type (^^Ec));
enum U { u1, u2 };
static_assert (!is_scoped_enum_type (^^U));
enum F : int { f1, f2 };
static_assert (!is_scoped_enum_type (^^F));
static_assert (!is_scoped_enum_type (^^Incomplete_struct));
struct S;
static_assert (!is_scoped_enum_type (^^S));
struct S { };
static_assert (!is_scoped_enum_type (^^S));
static_assert (!is_scoped_enum_type (^^int));
static_assert (!is_scoped_enum_type (^^int[]));
static_assert (!is_scoped_enum_type (^^int[2]));
static_assert (!is_scoped_enum_type (^^int[][2]));
static_assert (!is_scoped_enum_type (^^int[2][3]));
static_assert (!is_scoped_enum_type (^^int *));
static_assert (!is_scoped_enum_type (^^int &));
static_assert (!is_scoped_enum_type (^^int *&));
static_assert (!is_scoped_enum_type (^^int ()));
static_assert (!is_scoped_enum_type (^^int (*) ()));
static_assert (!is_scoped_enum_type (^^int (&) ()));
