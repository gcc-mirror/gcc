/* Verify that warnings about member initializers appear at the bad value,
   rather than on the last token of the final initializer.  */

// { dg-do compile }
// { dg-options "-Wconversion-null -fdiagnostics-show-caret" }

#define NULL ((void *)0) // { dg-error "invalid conversion from 'void\\*' to 'int'" }
/* { dg-begin-multiline-output "" }
 #define NULL ((void *)0)
              ~^~~~~~~~~~
               |
               void*
   { dg-end-multiline-output "" } */

class A
{
public:
  A();
  bool m_bool;
  int m_int;
  void *m_ptr;
};

A::A()
  : m_bool(NULL),
    m_int(NULL), // { dg-message "in expansion of macro 'NULL'" }
    m_ptr(NULL)
{
}

/* { dg-begin-multiline-output "" }
     m_int(NULL),
           ^~~~
   { dg-end-multiline-output "" } */
