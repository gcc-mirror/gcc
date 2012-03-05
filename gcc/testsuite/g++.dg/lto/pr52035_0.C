// PR c++/52035
// { dg-lto-do assemble }

template <typename T> struct QVector {
    typedef T* iterator;
    static void insert(int n);
    typedef int size_type;
};
template <typename T> void QVector<T>::insert(size_type n) {}
void error()
{
    int n;
    QVector<int>::insert(n);
}
