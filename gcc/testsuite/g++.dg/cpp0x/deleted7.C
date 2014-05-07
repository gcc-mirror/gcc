// PR c++/61080
// { dg-do compile { target c++11 } }
// { dg-options "-Wreturn-type" }

struct AAA
{
  int a1, a2, a3;
  void *p;
};

template <typename K, typename V>
class WeakMapPtr
{
  public:
    WeakMapPtr() : ptr(nullptr) {};
    bool init(AAA *cx);
  private:
    void *ptr;
    WeakMapPtr(const WeakMapPtr &wmp) = delete;
    WeakMapPtr &operator=(const WeakMapPtr &wmp) = delete;
};

template <typename K, typename V>
bool WeakMapPtr<K, V>::init(AAA *cx)
{
    ptr = cx->p;
    return true;
}

struct JSObject
{
  int blah;
  float meh;
};

template class WeakMapPtr<JSObject*, JSObject*>;
