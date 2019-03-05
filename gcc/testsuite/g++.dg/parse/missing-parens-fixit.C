// { dg-options "-fdiagnostics-show-caret" }

class t1
{
public:
  double length () const { return m_length; }
  double area (double width) const { return m_length * width; }

private:
  double m_length;
};

bool test_1 (const t1 &inst)
{
  return inst.length > 0.0; // { dg-error "did you forget the '\\(\\)'" }
  /* We expect a fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   return inst.length > 0.0;
          ~~~~~^~~~~~
                     ()
     { dg-end-multiline-output "" } */
}

bool test_2 (const t1 &inst)
{
  return inst.area > 0.0; // { dg-error "did you forget the '\\(\\)'" }
  /* "t1::area" has additional params, so we don't expect a fix-it hint.  */
  /* { dg-begin-multiline-output "" }
   return inst.area > 0.0;
          ~~~~~^~~~
     { dg-end-multiline-output "" } */
}
