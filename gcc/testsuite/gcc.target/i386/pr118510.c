/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapxf" } */

typedef struct cpp_num cpp_num;
struct cpp_num {
    int high;
    unsigned low;
    int overflow;
};
int num_rshift_n;
cpp_num num_lshift(cpp_num num) {
    num.low = num.low >> num_rshift_n | num.high << (32 - num_rshift_n);
    return num;
}
