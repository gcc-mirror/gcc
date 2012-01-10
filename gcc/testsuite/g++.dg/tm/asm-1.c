// { dg-do compile }
// { dg-options "-fgnu-tm -O1" }

template<class T> class shared_ptr {
public:
    shared_ptr()  {
      __asm__ ("");
    }
};
template<typename _Tp> class deque {
public:
    void push_back() {
      ::new _Tp();
    }
};
class Bar {
  __attribute__((transaction_callable)) void push();
  deque<shared_ptr<int> > events;
};
void Bar::push() {
  events.push_back();
}
