// { dg-do assemble }

struct test_box
    {
     void print(void);
    };

void test<class BOX> (test_box *);   // { dg-error "" } illegal code

class test_square
    {
      friend void test<class BOX> (test_box *); // { dg-error "" } does not match
    }



template <class BOX> void test(BOX *the_box)  // { dg-error "" } semicolon missing
    {x
    the_box->print();
    };

template void test<> (test_box *); // { dg-error "" }
