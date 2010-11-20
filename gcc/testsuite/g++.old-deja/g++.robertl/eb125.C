// { dg-do assemble }

struct test_box
    {
     void print(void);
    };

void test<class BOX> (test_box *);   // { dg-error "" } illegal code

class test_square
    {
      friend void test<class BOX> (test_box *); // { dg-error "" } does not match
    }						// { dg-error "after class definition" }



template <class BOX> void test(BOX *the_box)
    {x				// { dg-error "not declared in this scope" }
    the_box->print();		// { dg-error "before" }
    }

template void test<> (test_box *);
