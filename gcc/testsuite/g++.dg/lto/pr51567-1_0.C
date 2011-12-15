// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

struct _Deque_iterator {
    int* _M_cur;
    void foo() {}
};
class _Deque_base {
public:
    typedef _Deque_iterator iterator;
    iterator _M_impl;
};
class deque : public _Deque_base {
public:
    typedef _Deque_base::iterator iterator;
};
class OutputContextStack {
public:
    deque m_stack;
    deque::iterator m_stackPosition;
};
int main()
{
  OutputContextStack s;
  s.m_stackPosition.foo();
}

