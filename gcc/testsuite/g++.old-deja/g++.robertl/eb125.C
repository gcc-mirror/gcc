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

template void test<> (test_box *);     // gets bogus error - test is declared XFAIL *-*-*

