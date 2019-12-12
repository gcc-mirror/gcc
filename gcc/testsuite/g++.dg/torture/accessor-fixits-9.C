// PR c++/84993
// { dg-options "-fdiagnostics-show-caret" }

/* Misspelling (by omitting a leading "m_") of a private member for which
   there's a public accessor.

   We expect a fix-it hint suggesting the accessor.  */

class t1
{
public:
  int get_ratio () const { return m_ratio; }

private:
  int m_ratio;
};

int test (t1 *ptr_1)
{
  return ptr_1->ratio; // { dg-error "'class t1' has no member named 'ratio'; did you mean 'int t1::m_ratio'\\? \\(accessible via 'int t1::get_ratio\\(\\) const'\\)" }
  /* { dg-begin-multiline-output "" }
   return ptr_1->ratio;
                 ^~~~~
                 get_ratio()
     { dg-end-multiline-output "" } */
}


/* Misspelling of a private member for which there's a public accessor.

   We expect a fix-it hint suggesting the accessor.  */

class t2
{
public:
  int get_color () const { return m_color; }

private:
  int m_color;
};

int test (t2 *ptr_2)
{
  return ptr_2->m_colour; // { dg-error "'class t2' has no member named 'm_colour'; did you mean 'int t2::m_color'\\? \\(accessible via 'int t2::get_color\\(\\) const'\\)" }
  /* { dg-begin-multiline-output "" }
   return ptr_2->m_colour;
                 ^~~~~~~~
                 get_color()
     { dg-end-multiline-output "" } */
}


/* Misspelling of a private member via a subclass pointer, for which there's
   a public accessor in the base class.

   We expect a fix-it hint suggesting the accessor.  */

class t3 : public t2 {};

int test (t3 *ptr_3)
{
  return ptr_3->m_colour; // { dg-error "'class t3' has no member named 'm_colour'; did you mean 'int t2::m_color'\\? \\(accessible via 'int t2::get_color\\(\\) const'\\)" }
  /* { dg-begin-multiline-output "" }
   return ptr_3->m_colour;
                 ^~~~~~~~
                 get_color()
     { dg-end-multiline-output "" } */
}


/* Misspelling of a protected member, for which there's isn't a public
   accessor.

   We expect no fix-it hint; instead a message identifying where the
   data member was declared.  */

class t4
{
protected:
  int m_color; // { dg-message "declared protected here" }
};

int test (t4 *ptr_4)
{
  return ptr_4->m_colour; // { dg-error "'class t4' has no member named 'm_colour'; did you mean 'int t4::m_color'\\? \\(not accessible from this context\\)" }
  /* { dg-begin-multiline-output "" }
   return ptr_4->m_colour;
                 ^~~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */
}


/* Misspelling of a private member, for which the accessor is also private.

   We expect no fix-it hint; instead a message identifying where the
   data member was declared.  */

class t5
{
  int get_color () const { return m_color; }
  int m_color; // { dg-message "declared private here" }
};

int test (t5 *ptr_5)
{
  return ptr_5->m_colour; // { dg-error "'class t5' has no member named 'm_colour'; did you mean 'int t5::m_color'\\? \\(not accessible from this context\\)" }
  /* { dg-begin-multiline-output "" }
   return ptr_5->m_colour;
                 ^~~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   int m_color;
       ^~~~~~~
     { dg-end-multiline-output "" } */
}
