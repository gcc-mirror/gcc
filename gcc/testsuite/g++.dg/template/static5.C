// { dg-do compile }

// Origin: Mirek Fidler <cxl@ntllib.org>
//         Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/12932: ICE address of static function as template argument

struct Test {
    static void fun();
};

template <void (*fun)()>
void foo () { (*fun)(); }


template
void foo<Test::fun> ();
