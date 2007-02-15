/* https://bugzilla.redhat.com/bugzilla/show_bug.cgi?id=223576 */

/* SRA failed to canonicalize bit-field types, introducing type
   mismatches.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

struct A
{
  int a:16;
  /* These dummy bit-fields are here to prevent GCC 4.2+ from merging
     the bit-field compares into a single word compare, which disables
     SRA.  */
  int a2:16;
  int a3:16;
  int a4:16;
  int b:8;
  bool operator==(A const x) const
  {
    return (this->a == x.a && this->b == x.b);
  }
};

bool
foo (A const x, A const y)
{
  return x == y;
}
