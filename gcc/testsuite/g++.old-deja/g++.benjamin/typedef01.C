// Build don't link:
//980205 bkoz

//7.1.3 the typedef specifier


//p1
typedef int MILES, *KLICKSP;
MILES distance;
extern KLICKSP metricp;

//p2--can redefine to same type
typedef struct s { /* ... */ } s;
typedef int I;
typedef int I;
typedef I I;

//p3--cannot redefine to a different type in a given scope
class complex2 { /* ... */ };// ERROR - .*
typedef int complex2;// ERROR - .*
typedef int complex3;// ERROR - .*
class complex3 { /* ... */ };// ERROR - .*


//p4
/*
4 A typedef-name that names a class is a class-name (_class.name_).   If
  a  typedef-name is used 
  1) following the class-key in an elaborated-type-specifier 
  2) or in the class-head of a class declaration 
  3) or is used as the identifier in the declarator for a
  constructor or destructor  declaration 
  the program is ill-formed.  [Example:
*/
struct S {
  S();
  ~S();
};

typedef struct S T;

S a = T();                      // OK 
struct T * p;                   // ERROR - using typedef after struct

//case01
typedef bool short;// ERROR - .*
