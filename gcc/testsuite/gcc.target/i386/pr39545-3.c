/* PR target/39545 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2" } */

/* The struct is passed in memory with or without the flexible array
   member, so there is no ABI change, hence no note.  */

struct flex
{
  long a, b, c;
  int x[];
};

long f (struct flex s) { return s.a; }

/* { dg-bogus "the ABI of passing" "" { target *-*-* } 0 } */
