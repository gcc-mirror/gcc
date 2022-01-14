// C++ PR 70317
// { dg-do compile }
// { dg-prune-output "expected primary-expression" }

template<class T> class mytemplateclass
{
public:
  template<class U> void class_func() {}
  template<class U> static void class_func_static() {}
};

class myclass
{
public:
  int testint;
  template<class U> void class_func() {}
  template<class U> static void class_func_static() {}
};

template<class Y> void tests_func(mytemplateclass<Y> c, myclass c2)
{
  /* Dependent template accessors (ill-formed code).  */
  c.class_func<Y>(); // { dg-warning "keyword before dependent template name" }
  (&c)->class_func<Y>(); // { dg-warning "keyword before dependent template name" }
  mytemplateclass<Y>::class_func_static<Y>(); // { dg-warning "keyword before dependent template name" }

  /* Dependent template accessors (well-formed code).  */
  c.template class_func<Y>();
  (&c)->template class_func<Y>();
  mytemplateclass<Y>::template class_func_static<Y>();

  /* Non-dependent template accessors (well-formed code).  */
  c2.class_func<myclass>();
  (&c2)->class_func<myclass>();
  myclass::class_func_static<myclass>();
}

int main()
{
  mytemplateclass<myclass> c;
  myclass c2;
  tests_func<myclass>(c, c2);

  c2.testint = 53;
  /* Make sure this isn't treated as a template.  */
  bool testbool = c2.testint < 999 > 7;
  /* This probably will be treated as a template initially but it should still work.  */
  testbool = c2.testint < 123 > (50);
}
