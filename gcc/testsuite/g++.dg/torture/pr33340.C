void* operator new(__SIZE_TYPE__, void* __p) { }

struct auto_ptr {
        int* p;
        ~auto_ptr() { delete p; }
};

typedef void* T;
struct vector {
        void push_back(const T& __x) {
                ::new(0) T(__x);
                insert(__x);
        }
        void insert(const T& __x);
} v;

void g();
void f() {
        auto_ptr ap;
        if (ap.p) {
                ap.p = new int();
        }
        g();
        int* tmp = ap.p;
        ap.p = 0;
        v.push_back(tmp);
}
