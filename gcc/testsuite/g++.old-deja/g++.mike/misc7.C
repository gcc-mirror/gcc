// { dg-do assemble  }
// GROUPS passed MI
struct S1 { };

struct S2 : S1 { };
struct S3 : S1 { };

struct S4 : S3, S2 { };

struct S1 *p1;
struct S4 *p4;

void foobar ()
{
  p1 = p4;		// { dg-error "" } this is illegal// ERROR - .*
}
