// PR c++/40975

struct data_type
{
    // constructor required to reproduce compiler bug
    data_type() {}
};

struct ptr_type
{
    // array new as default argument required to reproduce compiler bug
    ptr_type (data_type* ptr = new data_type[1]) { delete[] ptr; }
};

ptr_type obj;
