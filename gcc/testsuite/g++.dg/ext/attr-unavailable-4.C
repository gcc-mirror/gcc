/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

struct B {
    virtual int foo() __attribute__((unavailable));
};

int main(void) {
  ((B*)0)->foo(); 		// { dg-error "unavailable" }
}
