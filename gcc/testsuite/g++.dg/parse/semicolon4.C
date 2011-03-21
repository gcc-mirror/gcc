// PR c++/46890
// { dg-do compile }

struct OK1
{
  int i;
} const *ok1_var;		// No complains

struct OK2;
extern OK2 ok2a_var;

struct OK2
{
  int i;
} const &ok2_var = ok2a_var;	// No complains

struct OK3
{
  int i;
} volatile (ok3_var);		// No complains

struct E1
{
  int i;
} const;			// { dg-error "qualifiers can only be specified for objects and functions" }

void foo (
struct E2
{				// { dg-error "types may not be defined in parameter types" }
  int i;
} volatile);

void bar (
struct E3
{				// { dg-error "types may not be defined in parameter types" }
  int i;
} const, int);
