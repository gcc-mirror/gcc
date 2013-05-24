// PR c++/56971

template <typename T>
class rp {
};

template <template <typename> class P>
struct b {
    template <class, template <typename> class FriendP>
    friend void f(b<FriendP> from);
};

template <class, template <typename> class P>
void f(b<P> from) {
}

int main() {
    b<rp> v;
    f<int>(v);
    return 0;
}
