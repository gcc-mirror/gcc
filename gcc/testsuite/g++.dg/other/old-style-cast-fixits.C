// { dg-options "-Wold-style-cast -fdiagnostics-show-caret" }

struct foo {};
struct bar { const foo *field; };

void test_1 (void *ptr)
{
  foo *f = (foo *)ptr; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)ptr;
                   ^~~
            ----------
            static_cast<foo *> (ptr)
     { dg-end-multiline-output "" } */
}

void test_2 (const foo *ptr)
{
  foo *f = (foo *)ptr; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)ptr;
                   ^~~
            ----------
            const_cast<foo *> (ptr)
     { dg-end-multiline-output "" } */
}

void test_3 (bar *ptr)
{
  foo *f = (foo *)ptr; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)ptr;
                   ^~~
            ----------
            reinterpret_cast<foo *> (ptr)
     { dg-end-multiline-output "" } */
}

void test_4 (bar *ptr)
{
  foo *f = (foo *)ptr->field; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)ptr->field;
                        ^~~~~
            -----------------
            const_cast<foo *> (ptr->field)
     { dg-end-multiline-output "" } */
}

void test_5 ()
{
  bar b_inst;
  foo *f = (foo *)&b_inst; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)&b_inst;
                    ^~~~~~
            --------------
            reinterpret_cast<foo *> (&b_inst)
     { dg-end-multiline-output "" } */
}

/* We don't offer suggestions for templates.  */

template <typename T>
void test_6 (void *ptr)
{
  foo *f = (foo *)ptr; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)ptr;
                   ^~~
     { dg-end-multiline-output "" } */
}

/* We don't offer suggestions where a single C++-style cast can't be
   used.  */

void test_7 (const void *ptr)
{
  foo *f = (foo *)ptr; // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)ptr;
                   ^~~
     { dg-end-multiline-output "" } */
}

/* Likewise, no single C++-style cast is usable here.  */

void test_8 (const bar &b_inst)
{
  foo *f = (foo *)&b_inst;  // { dg-warning "old-style cast" }
  /* { dg-begin-multiline-output "" }
   foo *f = (foo *)&b_inst;
                    ^~~~~~
     { dg-end-multiline-output "" } */
}
