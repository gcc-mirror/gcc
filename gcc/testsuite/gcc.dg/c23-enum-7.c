/* Test C23 enumerations with fixed underlying type.  Invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* An enum type specifier may only be used when the enum is defined, or in a
   declaration of the form "enum name enum-type-specifier;".  */
extern enum e1 : int; /* { dg-error "storage class specifier in empty declaration with 'enum' underlying type" } */
_Thread_local enum e2 : short; /* { dg-error "'_Thread_local' in empty declaration with 'enum' underlying type" } */
const enum e3 : long; /* { dg-error "type qualifier in empty declaration with 'enum' underlying type" } */
alignas (8) enum e4 : long; /* { dg-error "'alignas' in empty declaration with 'enum' underlying type" } */
inline enum e5 : unsigned; /* { dg-error "'inline' in empty declaration" } */
_Noreturn enum e6 : unsigned; /* { dg-error "'_Noreturn' in empty declaration" } */
auto enum e7 : unsigned; /* { dg-error "'auto' in file-scope empty declaration" } */
register enum e8 : unsigned; /* { dg-error "'register' in file-scope empty declaration" } */

/* When the enum is defined, some extra declaration specifiers are permitted,
   but diagnosed as useless.  */
extern enum e9 : int { E9 }; /* { dg-warning "useless storage class specifier in empty declaration" } */
_Thread_local enum e10 : short { E10 }; /* { dg-warning "useless '_Thread_local' in empty declaration" } */
const enum e11 : long { E11 }; /* { dg-warning "useless type qualifier in empty declaration" } */
alignas (8) enum e12 : long { E12 }; /* { dg-warning "useless '_Alignas' in empty declaration" } */

/* Nothing else may be declared with an enum type specifier for an enum not
   being defined in that declaration.  */
enum e13 : short x13; /* { dg-error "'enum' underlying type may not be specified here" } */
enum e14 : short f14 (); /* { dg-error "'enum' underlying type may not be specified here" } */
typeof (enum e15 : long) x15; /* { dg-error "'enum' underlying type may not be specified here" } */
int f16 (enum e16 : char p); /* { dg-error "'enum' underlying type may not be specified here" } */
int f17 (enum e17 : char); /* { dg-error "'enum' underlying type may not be specified here" } */
struct s18 { enum e18 : int x; }; /* { dg-error "'enum' underlying type may not be specified here" } */

/* But those are OK if the enum content is defined.  */
enum e19 : short { E19 } x19;
enum e20 : long { E20 } f20 ();
typeof (enum e21 : long { E21 }) x21;
int f22 (enum e22 : long long { E22 } p);
int f23 (enum e23 : long long { E23 } p);
struct s24 { enum e24 : int { E24 } x; };

/* Incompatible kinds of tags in the same scope are errors.  */
struct s25;
enum s25 : int; /* { dg-error "wrong kind of tag" } */
struct s26;
enum s26 : int { E26 }; /* { dg-error "wrong kind of tag" } */
struct s27 { int x; };
enum s27 : int; /* { dg-error "wrong kind of tag" } */
struct s28 { int x; };
enum s28 : int { E28 }; /* { dg-error "wrong kind of tag" } */
union u29;
enum u29 : int; /* { dg-error "wrong kind of tag" } */
union u30;
enum u30 : int { E30 }; /* { dg-error "wrong kind of tag" } */
union u31 { int x; };
enum u31 : int; /* { dg-error "wrong kind of tag" } */
union u32 { int x; };
enum u32 : int { E32 }; /* { dg-error "wrong kind of tag" } */

/* If an enum has a fixed underlying type, that must be given when defining the
   enum.  */
enum e33 : short;
enum e33 { E33 }; /* { dg-error "'enum' declared with but defined without fixed underlying type" } */

/* An enum defined without a fixed underlying type cannot then be declared with
   one.  */
enum e34 { E34A = -__INT_MAX__, E34B = __INT_MAX__ };
enum e34 : int; /* { dg-error "'enum' declared both with and without fixed underlying type" } */

/* An enum with a fixed underlying type cannot be declared with an incompatible
   fixed underlying type.  */
enum e35 : int;
enum e35 : unsigned int; /* { dg-error "'enum' underlying type incompatible with previous declaration" } */
enum e36 : int;
enum e36 : unsigned int { E36 }; /* { dg-error "'enum' underlying type incompatible with previous declaration" } */
enum e37 : unsigned int { E37 };
enum e37 : int; /* { dg-error "'enum' underlying type incompatible with previous declaration" } */

/* Enumeration constants must fit in the fixed underlying type.  */
enum e38 : unsigned char { E38 = (unsigned long long)((unsigned char) -1) + 1 }; /* { dg-error "enumerator value outside the range of underlying type" } */
enum e39 : unsigned int { E39 = -1 }; /* { dg-error "enumerator value outside the range of underlying type" } */
enum e40 : int { E40 = __INT_MAX__, E40A }; /* { dg-error "overflow in enumeration values" } */
enum e41 : unsigned int { E41 = (unsigned int) -1, E41A }; /* { dg-error "overflow in enumeration values" } */
enum e42 : bool { E42 = 2 }; /* { dg-error "enumerator value outside the range of underlying type" } */
enum e43 : bool { E43 = 1, E43A }; /* { dg-error "overflow in enumeration values" } */

/* The underlying type must be an integer type, not itself an enum (or
   bit-precise) type.  */
enum e44 : double; /* { dg-error "invalid 'enum' underlying type" } */
typedef int T;
enum e45 : T;
typedef int *TP;
enum e46 : TP; /* { dg-error "invalid 'enum' underlying type" } */
enum e47 : enum e45; /* { dg-error "invalid 'enum' underlying type" } */
enum e48 : const; /* { dg-error "no 'enum' underlying type specified" } */
/* 'restrict' is not valid on integer types.  */
enum e49 : int restrict; /* { dg-error "invalid use of 'restrict'" } */
