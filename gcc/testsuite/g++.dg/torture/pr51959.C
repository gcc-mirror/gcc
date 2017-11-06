// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

namespace std {
    typedef __SIZE_TYPE__ size_t;
}
inline void* operator new(std::size_t, void* __p) throw() {
    return __p;
}
template <typename T> class QTypeInfo {
};
enum { Q_COMPLEX_TYPE = 0,     Q_PRIMITIVE_TYPE = 0x1,     Q_STATIC_TYPE = 0,     Q_MOVABLE_TYPE = 0x2,     Q_DUMMY_TYPE = 0x4 };
template<typename Enum> class QFlags {
    int i;
    inline QFlags(Enum f) : i(f) { }
};
class __attribute__((visibility("default"))) QSize {
public:
    bool isEmpty() const;
    friend inline bool operator==(const QSize &, const QSize &);
    int wd;
    int ht;
};
template<> class QTypeInfo<QSize > {
public:
    enum {
	isComplex = (((Q_MOVABLE_TYPE) & Q_PRIMITIVE_TYPE) == 0), isStatic = (((Q_MOVABLE_TYPE) & (Q_MOVABLE_TYPE | Q_PRIMITIVE_TYPE)) == 0), isLarge = (sizeof(QSize)>sizeof(void*)), isPointer = false, isDummy = (((Q_MOVABLE_TYPE) & Q_DUMMY_TYPE) != 0) };
};
class __attribute__((visibility("default"))) QBasicAtomicInt {
public:
    inline bool operator!=(int value) const     { }
};
struct __attribute__((visibility("default"))) QListData {
    struct Data {
	QBasicAtomicInt ref;
    };
    void **append();
};
template <typename T> class QList {
    struct Node {
	void *v;
    };
    union {
	QListData p;
	QListData::Data *d;
    };
public:
    void append(const T &t);
    inline void push_back(const T &t) {
	append(t);
    }
    void node_construct(Node *n, const T &t);
    void node_destruct(Node *n);
};
template <typename T> inline void QList<T>::node_construct(Node *n, const T &t) {
    if (QTypeInfo<T>::isLarge || QTypeInfo<T>::isStatic) n->v = new T(t);
    else if (QTypeInfo<T>::isComplex) new (n) T(t);
}
template <typename T> inline void QList<T>::node_destruct(Node *n) {
}
template <typename T>  void QList<T>::append(const T &t) {
    if (d->ref != 1) {
	try {
	}
	catch (...) {
	}
	if (QTypeInfo<T>::isLarge || QTypeInfo<T>::isStatic) {
	}
	else {
	    Node *n, copy;
	    node_construct(&copy, t);
	    try {                 n = reinterpret_cast<Node *>(p.append());;             }
	    catch (...) {                 node_destruct(&copy);                 throw;             }
	    *n = copy;
	}
    }
};
void virtual_hook(QSize sz, QList<QSize> &arg)
{
  arg.push_back(sz);
}
