// PR37314 ice-on-valid-code, from w.doeringer
template <typename T>
class Cdeque {
    typedef T *pointer;
    class iterator {
        typedef typename Cdeque<T>::pointer pointer;
        pointer operator->();
    };
};
template <typename T> T* Cdeque<T>::iterator::operator->() { }


