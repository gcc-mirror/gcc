// { dg-options "-fdiagnostics-show-caret" }

class t1
{
public:
  int& get_color () { return m_color; }
  int& get_shape () { return m_shape; }

private:
  int m_color; // { dg-line color_decl }
  int m_shape; // { dg-line shape_decl }
};

int test_const_ptr (const t1 *ptr)
{
  return ptr->m_color; // { dg-error ".int t1::m_color. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ptr->m_color;
               ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "declared private here" "" { target *-*-* } color_decl }
  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  /* We shouldn't issue a suggestion: the accessor is non-const, and we
     only have a const ptr.  */
}

int test_const_reference (const t1 &ref)
{
  return ref.m_shape; // { dg-error ".int t1::m_shape. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ref.m_shape;
              ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "declared private here" "" { target *-*-* } shape_decl }
  /* { dg-begin-multiline-output "" }
   int m_shape;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  /* We shouldn't issue a suggestion: the accessor is non-const, and we
     only have a const ptr.  */
}
