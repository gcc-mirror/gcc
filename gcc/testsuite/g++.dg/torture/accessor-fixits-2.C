// { dg-options "-fdiagnostics-show-caret" }

/* Test of accessors that return references.  */

class t1
{
public:
  int& get_color () { return m_color; }
  int& get_shape () { return m_shape; }

private:
  int m_color;

protected:
  int m_shape;
};

int test_access_t1_color (t1 &ref)
{
  return ref.m_color; // { dg-error ".int t1::m_color. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ref.m_color;
              ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "declared private here" "" { target *-*-* } 12 }
  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_color. can be accessed via .int& t1::get_color\\(\\)." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ref.m_color;
              ^~~~~~~
              get_color()
     { dg-end-multiline-output "" } */
}

int test_access_t1_shape (t1 &ref)
{
  return ref.m_shape; // { dg-error ".int t1::m_shape. is protected within this context" }
  /* { dg-begin-multiline-output "" }
   return ref.m_shape;
              ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "declared protected here" "" { target *-*-* } 15 }
  /* { dg-begin-multiline-output "" }
   int m_shape;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_shape. can be accessed via .int& t1::get_shape\\(\\)." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ref.m_shape;
              ^~~~~~~
              get_shape()
     { dg-end-multiline-output "" } */
}

int test_deref_t1_color (t1 *ptr)
{
  return ptr->m_color; // { dg-error ".int t1::m_color. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ptr->m_color;
               ^~~~~~~
     { dg-end-multiline-output "" } */


  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_color. can be accessed via .int& t1::get_color\\(\\)." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ptr->m_color;
               ^~~~~~~
               get_color()
     { dg-end-multiline-output "" } */
}

int test_deref_t1_shape (t1 *ptr)
{
  return ptr->m_shape; // { dg-error ".int t1::m_shape. is protected within this context" }
  /* { dg-begin-multiline-output "" }
   return ptr->m_shape;
               ^~~~~~~
     { dg-end-multiline-output "" } */


  /* { dg-begin-multiline-output "" }
   int m_shape;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_shape. can be accessed via .int& t1::get_shape\\(\\)." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ptr->m_shape;
               ^~~~~~~
               get_shape()
     { dg-end-multiline-output "" } */
}
