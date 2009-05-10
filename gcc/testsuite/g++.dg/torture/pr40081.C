struct Atomic_t {
    Atomic_t(int i) : val(i) { }
    volatile int val;
};
class RefCount {
public:
    RefCount(Atomic_t c) : m_count(c)  { }
    Atomic_t m_count;
};
class IntrusiveCountableBase {
    RefCount m_useCount;
protected:
    IntrusiveCountableBase();
};
IntrusiveCountableBase::IntrusiveCountableBase() : m_useCount(0)  { }

