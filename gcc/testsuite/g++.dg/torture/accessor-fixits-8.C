// { dg-options "-fdiagnostics-show-caret" }

class t1
{
public:
  int get_doubled_field () const { return m_field * 2; }
  int get_guarded_field_1 () const { if (m_field) return m_field; else return 42; }
  int get_guarded_field_2 () const { return m_field ? m_field : 42; }
  int with_unreachable () const { __builtin_unreachable (); return m_field; }
  void no_return () { }

private:
  int m_field; // { dg-line field_decl }
};

int test (t1 *ptr)
{
  return ptr->m_field; // { dg-error ".int t1::m_field. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ptr->m_field;
               ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "declared private here" "" { target *-*-* } field_decl }
  /* { dg-begin-multiline-output "" }
   int m_field;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  /* We shouldn't issue a suggestion: none of the member functions are suitable returns.  */
}
