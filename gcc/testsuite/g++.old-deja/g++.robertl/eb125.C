// This is a crash test; we don't care how many normal errors we get.
// excess errors test - XFAIL *-*-*

struct test_box
    {
     void print(void);
    };

void test<class BOX> (test_box *);   // ERROR - illegal code

class test_square
    {
      friend void test<class BOX> (test_box *); // ERROR - does not match
    }



template <class BOX> void test(BOX *the_box)  // ERROR - semicolon missing
    {
    the_box->print();
    };

template void test<> (test_box *);
