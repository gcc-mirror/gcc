// Error: Internal Compiler Error in GCC 2.7.2 and EGCS 1998/05/28 snapshot.

#include <iostream.h>

class some_base
        {
public:
        class base_func_args;
        virtual void func(base_func_args &) = 0; // ERROR - referenced below
        };

class some_base::base_func_args
        {
public:
        int i;
        };

class some_derived : public some_base
        {
public:
        class derived_func_args;
        void func(derived_func_args &);
        };


class derived_func_args : public some_base::base_func_args
        {
public:
        float f;
        };

class some_derived::func(derived_func_args &a)  // ERROR - illegal member syntax
        {
        cout << a.i << ' ' << a.f << endl;
        }

int
main()
        {
	some_derived d;                     // ERROR - abstract class
        some_derived::derived_func_args dfa; // ERROR - incomplete class
        some_base *b = &d;

        dfa.i = 10;
        dfa.f = 20;
        b->func(dfs);                       // ERROR - dfs not declared
        return 0;
        }
