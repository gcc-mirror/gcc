// PR tree-optimization/95638
// { dg-do run }
// { dg-options "-O2 -std=c++14" }

#include <cassert>
#include <cstdio>
#include <cstdint>
#include <memory>
#include <type_traits>
#include <utility>

template <typename Derived>
class intrusive_ref_counter
{
public:
    intrusive_ref_counter() = default;
    
    intrusive_ref_counter(intrusive_ref_counter const&) :
        _counter(0)
    {
    }

    friend void intrusive_ptr_add_ref(const intrusive_ref_counter<Derived>* p)
    {
        ++p->_counter;
    }
    
    friend void intrusive_ptr_release(const intrusive_ref_counter<Derived>* p)
    {
        if (--p->_counter == 0)
        {
            delete static_cast<const Derived*>(p);
        }
    }
    
private:
    mutable int _counter = 0;
};

template <typename T>
class intrusive_ptr
{
public:
    intrusive_ptr() = default;

    intrusive_ptr(T* p): px(p)
    {
        if (px != 0) intrusive_ptr_add_ref(px);
    }

    intrusive_ptr(intrusive_ptr const & rhs): px(rhs.px)
    {
        if (px != 0) intrusive_ptr_add_ref(px);
    }

    ~intrusive_ptr()
    {
        if (px != 0) intrusive_ptr_release(px);
    }

    intrusive_ptr(intrusive_ptr && rhs) : px(rhs.px)
    {
        rhs.px = 0;
    }

    explicit operator bool() const
    {
        return px != 0;
    }
    
private:
    T* px = nullptr;
};

template <typename T, uint32_t MaxSize = 1>
class Storage
{
public:
    Storage() = default;

    Storage(Storage&& other)
    {
        for (int i = 0; i < other._size; ++i)
        {
            new (data() + i) T(std::move(other.data()[i]));
            ++_size;
        }
    }

    ~Storage()
    {
        for (int i = 0; i < _size; ++i)
        {
            data()[i].~T();
        }
    }

    void push_back(const T& value)
    {
        assert(_size < MaxSize);

        new (data() + _size) T(value);
        ++_size;
    }
    
    T& operator[](size_t idx)
    {
        assert(idx < _size);
        return data()[idx];
    }

private:
    T* data()
    {
        return reinterpret_cast<T*>(&_data);
    }

private:
    uint32_t _size = 0;
    std::aligned_storage_t<sizeof(T[MaxSize])> _data;
};

struct Item: intrusive_ref_counter<Item>
{
};

using Collection = Storage<intrusive_ptr<Item>>;

struct Holder
{
    __attribute__ ((noinline)) Holder(Collection collection) : collection(std::move(collection)) {}

    int64_t dummy = 0; // this is needed to reproduce the issue.
    Collection collection;
};

int main()
{
    Collection collection;
    collection.push_back(intrusive_ptr<Item>(new Item()));

    Holder holder(std::move(collection));

    auto item = holder.collection[0];
    
    if (!item)
      __builtin_abort ();

    return 0;
}
