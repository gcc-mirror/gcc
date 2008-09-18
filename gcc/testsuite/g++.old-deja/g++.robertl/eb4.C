// { dg-do assemble  }
// Error: Internal Compiler Error in GCC 2.7.2 and EGCS 1998/05/28 snapshot.

#include <iostream>

class some_base
        {
public:
        class base_func_args;
        virtual void func(base_func_args &) = 0; // { dg-message "note" } referenced below
        };

class some_base::base_func_args
        {
public:
        int i;
        };

class some_derived : public some_base
        {  // { dg-message "note" }
public:
        class derived_func_args;
        void func(derived_func_args &);
        };


class derived_func_args : public some_base::base_func_args
        {
public:
        float f;
        };

class some_derived::func(derived_func_args &a)  // { dg-error "does not name a type" "type" } illegal member syntax
// { dg-error "expected" "exp" { target *-*-* } 33 }
        {
        std::cout << a.i << ' ' << a.f << std::endl;
        }

int
main()
        {
	some_derived d;                     // { dg-error "abstract type" } 
        some_derived::derived_func_args dfa; // { dg-error "incomplete type" } 
        some_base *b = &d;

        dfa.i = 10;
        dfa.f = 20;
        b->func(dfs);                       // { dg-error "'dfs' was not declared" } 
        return 0;
        }
