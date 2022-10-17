/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fanalyzer-checker=malloc -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

#include <cstdlib>

struct Base
{
    virtual void allocate ();
    virtual void deallocate (); 
};

struct Derived: public Base
{
    int *ptr;
    void allocate ()
    {
        ptr = (int*)malloc(sizeof(int));
    }
    void deallocate () 
    { 
        free(ptr);
    }
};

void test()
{
    Derived D;
    Base B, *base_ptr;
    base_ptr = &D;

    D.allocate();
    base_ptr->deallocate();
    int n = *D.ptr;   /* { dg-warning "use after 'free' of 'D.Derived::ptr'" } */
}

/* use after 'free'  */
/* { dg-begin-multiline-output "" }
   NN |     int n = *D.ptr;
      |         ^
  'void test()': events 1-2
    |
    |   NN | void test()
    |      |      ^~~~
    |      |      |
    |      |      (1) entry to 'test'
    |......
    |   NN |     D.allocate();
    |      |     ~~~~~~~~~~~~
    |      |               |
    |      |               (2) calling 'Derived::allocate' from 'test'
    |
    +--> 'virtual void Derived::allocate()': events 3-4
           |
           |   NN |     void allocate ()
           |      |          ^~~~~~~~
           |      |          |
           |      |          (3) entry to 'Derived::allocate'
           |   NN |     {
           |   NN |         ptr = (int*)malloc(sizeof(int));
           |      |                     ~~~~~~~~~~~~~~~~~~~
           |      |                           |
           |      |                           (4) allocated here
           |
    <------+
    |
  'void test()': events 5-6
    |
    |   NN |     D.allocate();
    |      |     ~~~~~~~~~~^~
    |      |               |
    |      |               (5) returning to 'test' from 'Derived::allocate'
    |   NN |     base_ptr->deallocate();
    |      |     ~~~~~~~~~~~~~~~~~~~~~~
    |      |                         |
    |      |                         (6) calling 'Derived::deallocate' from 'test'
    |
    +--> 'virtual void Derived::deallocate()': events 7-8
           |
           |   NN |     void deallocate ()
           |      |          ^~~~~~~~~~
           |      |          |
           |      |          (7) entry to 'Derived::deallocate'
           |   NN |     {
           |   NN |         free(ptr);
           |      |         ~~~~~~~~~
           |      |             |
           |      |             (8) freed here
           |
    <------+
    |
  'void test()': events 9-10
    |
    |   NN |     base_ptr->deallocate();
    |      |     ~~~~~~~~~~~~~~~~~~~~^~
    |      |                         |
    |      |                         (9) returning to 'test' from 'Derived::deallocate'
    |   NN |     int n = *D.ptr;
    |      |         ~                
    |      |         |
    |      |         (10) use after 'free' of 'D.Derived::ptr'; freed at (8)
    |
   { dg-end-multiline-output "" } */

