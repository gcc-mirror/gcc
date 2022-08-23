// PR c++/100502

template <class T>
struct EnumeratorRange {
  struct Iterator {
    EnumeratorRange range_;

    friend void f(Iterator i) {
      i.range_.end_reached_;
      i.range_.EnumeratorRange::end_reached_;
      &i.range_.end_reached_;
      &i.range_.EnumeratorRange::end_reached_;
    }
  };

 private:
  bool end_reached_;
#if DECLARE_FRIEND
  friend void f(Iterator);
#endif
};

int main() {
  EnumeratorRange<int>::Iterator i;
  f(i);
}
