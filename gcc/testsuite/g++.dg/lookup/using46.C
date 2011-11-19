// PR c++/51141
// { dg-do compile }
// { dg-options "-fpermissive -w -Werror" }

typedef int size_t;
template < size_t, size_t > struct AlignedBuffer {};

template < typename > class VectorBufferBase
{
public:
    allocateBuffer (size_t) {
    }
    buffer () {
    }
    *m_buffer;
    size_t m_capacity;
};

template < typename T, size_t > class VectorBuffer:VectorBufferBase < T >
{
    typedef VectorBufferBase < T > Base;

public:
    VectorBuffer () {
    }
    allocateBuffer (size_t) {
        m_capacity = 0;
    }
    Base::buffer;
    Base::m_buffer;
    Base::m_capacity;
    size_t m_inlineBufferSize;

    AlignedBuffer < 0, __alignof__ (T) > m_inlineBuffer;
};

template < typename T, size_t > class Vector
{
    typedef VectorBuffer < T,
            0 > Buffer;
public:
    void shrinkCapacity (size_t);

    clear () {
        shrinkCapacity (0);
    }
    Buffer m_buffer;
};

template < typename T, size_t inlineCapacity > void Vector < T,
         inlineCapacity >::shrinkCapacity (size_t)
{
    m_buffer.allocateBuffer (0);
}

struct PatternDisjunction;
struct YarrPattern {
    reset () {
        m_disjunctions.clear ();
    }
    Vector < PatternDisjunction *, 0 > m_disjunctions;
};
