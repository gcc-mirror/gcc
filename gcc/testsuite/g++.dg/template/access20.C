// PR c++/29470

template <typename T> struct B
{
        protected:
        T v;			// { dg-message "protected" }
};
template <typename T> struct D : B<T>
{
        protected:
        using B<T>::v;
};
int main()
{
        D<int> d;
        d.v = 0;		// { dg-error "context" }
        return 0;
}
