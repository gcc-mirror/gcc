class foo
{
public:
  double get_ratio() const;

private:
  double m_ratio; // { dg-line field_decl }
};

double
foo::get_ratio() const
{
  return m_ratio;
}

void test(foo *ptr)
{
  if (ptr->m_ratio >= 0.5) // { dg-error "'double foo::m_ratio' is private within this context" }
    ;
  // { dg-message "declared private here" "" { target *-*-* } field_decl }
  // { dg-message "'double foo::m_ratio' can be accessed via 'double foo::get_ratio\\(\\) const'" "" { target *-*-* } .-3 }
}
