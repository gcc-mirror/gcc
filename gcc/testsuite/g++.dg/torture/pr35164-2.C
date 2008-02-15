struct __shared_count {
    __shared_count() { _M_pi = new int; }
    int * _M_pi;
};
template<typename _Tp>
class __shared_ptr {
public:
    __shared_ptr(_Tp* __p);
    void reset(int * __p) {
        __shared_ptr(__p).swap(*this);
    }
    void swap(__shared_ptr<_Tp>& __other) {
        __other._M_refcount._M_pi = _M_refcount._M_pi;
    }
    __shared_count _M_refcount;
};
template<typename _Tp> class shared_ptr : public __shared_ptr<_Tp> {};
int main() {
    for (shared_ptr<int> *iter;;)
    {
        try {
            (iter++)->reset(new int);
        }
        catch (...) {
        }
    }
}
