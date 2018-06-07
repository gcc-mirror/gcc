// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attribute deprecated from a primary template declared
// with it.
// { dg-do compile }
// { dg-options "-Wall -fdump-tree-optimized" }

struct Special;

template <class T>
void fdeprecated_primary ();

// The primary isn't deprecated at this point so the declaration
// of its specialization should not be diagnosed.
template <>
void fdeprecated_primary<Special> ();   // { dg-bogus "deprecated" }

template <class T>
void __attribute__ ((deprecated))
fdeprecated_primary ();

void use_primary ()
{
  // Verify that uses of the now deprecacted primary are diagnosed.
  fdeprecated_primary<void>();          // { dg-warning "deprecated" "bug 84542" { xfail *-*-* } }
  fdeprecated_primary<int>();           // { dg-warning "deprecated" "bug 84542" { xfail *-*-* } }
}

void use_special ()
{
  // Verify that the use of the non-deprecated specializatoin
  // is not diagnosed.
  fdeprecated_primary<Special>();
}
