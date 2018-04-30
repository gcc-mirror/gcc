// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

namespace std {
    typedef __SIZE_TYPE__ size_t;
}
inline void* operator new(std::size_t, void* __p) throw() {
    return __p;
}
class Noncopyable  { };
struct CollectorCell { };
template<typename T> class PassRefPtr {
public:
    T* releaseRef() const { }
};
template <typename T> class NonNullPassRefPtr {
public:
    template <class U> NonNullPassRefPtr(const PassRefPtr<U>& o)
	: m_ptr(o.releaseRef()) { }
    mutable T* m_ptr;
};
struct ClassInfo;
class JSValue { };
JSValue jsNull();
class Structure;
class JSGlobalData {
    static void storeVPtrs();
};
class JSCell : public Noncopyable {
    friend class JSObject;
    friend class JSGlobalData;
    virtual ~JSCell();
};
class JSObject : public JSCell {
public:
    explicit JSObject(NonNullPassRefPtr<Structure>);
    static PassRefPtr<Structure> createStructure(JSValue prototype) { }
};
class JSByteArray : public JSObject {
    friend class JSGlobalData;
    enum VPtrStealingHackType { VPtrStealingHack };
    JSByteArray(VPtrStealingHackType)
	: JSObject(createStructure(jsNull())), m_classInfo(0) { }
    const ClassInfo* m_classInfo;
};
void JSGlobalData::storeVPtrs() {
    CollectorCell cell;
    void* storage = &cell;
    JSCell* jsByteArray = new (storage) JSByteArray(JSByteArray::VPtrStealingHack);
    jsByteArray->~JSCell();
}
