// PR c++/14089
// { dg-do compile }
//
// C++ front end generated assignment between types that were not
// compatible in any sense visible to the optimizers.

struct pair {
    typedef void (pair::*fp)();
    int first;
    pair::fp second;
    pair(const int& a, const pair::fp& b) : first(a), second(b) {}
    void f(const int& a, const pair::fp& b) { first = a; second = b; }
};

void op() {
    pair(5, pair::fp());
}
