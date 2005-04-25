// { dg-do compile }
// { dg-options "-O2" }
// We used to ICE in compare_values as the types for a comparison
// were not the same kind of types.

struct class1
{
  virtual ~class1 ();
};
struct class2 :  class1 { };

void f(class1 * oo)
{
  class2 * oj = dynamic_cast <class2 *>(oo) ;
  if (oj)
    delete oo;
}
