// PR c++/89831

struct Q { 
    int operator[](int i) { return 0; }
    int operator[](int i) const { return 0; }
};

struct Base {
    Q x;
};
struct X : public Base {
    template <typename T>
    void f(T) const {
        int q = Base::x[0];
    }   
};
int main() { X().f(3); }
