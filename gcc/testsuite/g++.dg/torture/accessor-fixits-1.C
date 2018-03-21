// { dg-options "-fdiagnostics-show-caret" }

class t1
{
public:
  int get_color () const { return m_color; }
  int get_shape () const { return m_shape; }

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

  // { dg-message "declared private here" "" { target *-*-* } 10 }
  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_color. can be accessed via .int t1::get_color\\(\\) const." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ref.m_color;
              ^~~~~~~
              get_color()
     { dg-end-multiline-output "" } */
}

int test_access_const_t1_color (const t1 &ref)
{
  return ref.m_color; // { dg-error ".int t1::m_color. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ref.m_color;
              ^~~~~~~
     { dg-end-multiline-output "" } */


  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_color. can be accessed via .int t1::get_color\\(\\) const." "" { target *-*-* } .-12 }
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

  // { dg-message "declared protected here" "" { target *-*-* } 13 }
  /* { dg-begin-multiline-output "" }
   int m_shape;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  // { dg-message "field .int t1::m_shape. can be accessed via .int t1::get_shape\\(\\) const." "" { target *-*-* } .-12 }
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

  // { dg-message "field .int t1::m_color. can be accessed via .int t1::get_color\\(\\) const." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ptr->m_color;
               ^~~~~~~
               get_color()
     { dg-end-multiline-output "" } */
}

int test_deref_const_t1_color (const t1 *ptr)
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

  // { dg-message "field .int t1::m_color. can be accessed via .int t1::get_color\\(\\) const." "" { target *-*-* } .-12 }
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

  // { dg-message "field .int t1::m_shape. can be accessed via .int t1::get_shape\\(\\) const." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ptr->m_shape;
               ^~~~~~~
               get_shape()
     { dg-end-multiline-output "" } */
}

/* Example of public inheritance.  */

class t2 : public t1
{
};

int test_deref_t2_color (t2 *ptr)
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

  // { dg-message "field .int t1::m_color. can be accessed via .int t1::get_color\\(\\) const." "" { target *-*-* } .-12 }
  /* { dg-begin-multiline-output "" }
   return ptr->m_color;
               ^~~~~~~
               get_color()
     { dg-end-multiline-output "" } */
}

/* Example of private inheritance.  */

class t3 : private t1
{
};

int test_deref_t3_color (t3 *ptr)
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

  /* We shouldn't provide a fix-it hint for this case due to the
     private inheritance.  */
}

/* Example of non-public "accessor".  */

class t4
{
  int m_field;
  int get_field () { return m_field; }
};

int test_deref_t4_field (t4 *ptr)
{
  return ptr->m_field; // { dg-error ".int t4::m_field. is private within this context" }
  /* { dg-begin-multiline-output "" }
   return ptr->m_field;
               ^~~~~~~
     { dg-end-multiline-output "" } */

  /* { dg-begin-multiline-output "" }
   int m_field;
       ^~~~~~~
     { dg-end-multiline-output "" } */

  /* We shouldn't provide a fix-it hint for this case, as the accessor is
     itself private.  */
}
