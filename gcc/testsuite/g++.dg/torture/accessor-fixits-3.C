class foo
{
public:
  static foo& get_singleton () { return s_singleton; }

private:
  static foo s_singleton;
};

foo & test_access_singleton ()
{
  return foo::s_singleton; // { dg-error ".foo foo::s_singleton. is private within this context" }
  // { dg-message "declared private here" "" { target *-*-* } 7 }
  // We don't yet support generating a fix-it hint for this case.
}
