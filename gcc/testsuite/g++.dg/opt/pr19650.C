// { dg-options "-O1 -w -fpermissive" }
// { dg-do "run" }
// Tests the fold bug described in PR 19650.
#include <stdio.h>
#include <stdlib.h>
#define test(a) ((a) ? 1 : 0)

typedef int (*arg_cmp_func)();

class Item_func 
{
public:
    enum Functype { UNKNOWN_FUNC, EQ_FUNC, EQUAL_FUNC };
    virtual enum Functype functype() const { return UNKNOWN_FUNC; }
};

class Item_bool_func2 : public Item_func
{
public:
    virtual enum Functype functype() const { return EQUAL_FUNC; }
};

class Arg_comparator 
{
public:
    Item_bool_func2 *owner;
    arg_cmp_func func;
    static arg_cmp_func comparator_matrix[4][2];

    int Arg_comparator::set_compare_func(Item_bool_func2 *item, int type)
    {
        owner = item;

        /****************** problematic line is here ************************/

        func = comparator_matrix[type][test(owner->functype() == Item_func::EQUAL_FUNC)];
        return 0;
    }
};

int compare_string() { return 0; }
int compare_e_string() { return 0; }
int compare_real() { return 0; }
int compare_e_real() { return 0; }
int compare_int_signed() { return 0; }
int compare_e_int() { return 0; }
int compare_row() { return 0; }
int compare_e_row() { return 0; }

arg_cmp_func Arg_comparator::comparator_matrix[4][2] =
    {{&compare_string,     &compare_e_string},
     {&compare_real,       &compare_e_real},
     {&compare_int_signed, &compare_e_int},
     {&compare_row,        &compare_e_row}};

void myfunc (const char*p, arg_cmp_func f1, arg_cmp_func f2) __attribute__((noinline));
void myfunc (const char*p, arg_cmp_func f1, arg_cmp_func f2)
{
    if (f1!=f2)
      abort ();
}

int main()
{
    Arg_comparator cmp;
    Item_bool_func2 equal_func;

    cmp.set_compare_func(&equal_func, 0);
    myfunc("cmp.func is %p (expected %p)\n", cmp.func, &compare_e_string);
}

