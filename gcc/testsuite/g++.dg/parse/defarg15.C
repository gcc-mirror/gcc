// PR c++/44991

class bar {
    void foo(bool a = 3 < 2, bool b = true) {}
};
