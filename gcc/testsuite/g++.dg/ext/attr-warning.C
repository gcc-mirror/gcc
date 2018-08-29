// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attribute warning from a primary template declared with
// it.
// { dg-do compile }
// { dg-options "-Wall" }

struct Special;

// Primary has no attributes here.
template <class T>
void fwarn_primary ();

// Uses of the primary template, including declarations of its
// specializations, should not be diagnosed until after it has
// been redeclared with attribute warning.
template <>
void fwarn_primary<Special> ();

void use_primary_before_warning ()
{
  // Verify that uses of the primary are not diagnosed.
  fwarn_primary<char>();
  fwarn_primary<short>();
}

// Redeclare the primary with attribute warning.
template <class T>
void __attribute__ ((warning ("primary")))
fwarn_primary ();

// Attribute warning is special in that it only warns for functions
// that are actually used, not those that are only declared.
template <>
void fwarn_primary<double> ();

void use_primary_after_warning ()
{
  // Verify that uses of the redeclared primary are diagnosed.
  fwarn_primary<int>();           // { dg-warning "primary" }
  fwarn_primary<long>();          // { dg-warning "primary" }
}

void use_special ()
{
  // Verify that the use of the specializatoin is not diagnosed.
  fwarn_primary<Special>();
}
