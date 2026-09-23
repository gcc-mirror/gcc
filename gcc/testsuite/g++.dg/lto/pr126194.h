#pragma once

using Size = decltype(sizeof(0));

struct A
{
    void (*const f_)(void**) noexcept;
    void** const p_;
    Size const a_;

    A(void** pp, void (*f)(void**) noexcept, Size a) noexcept
        : f_(f), p_(pp), a_(a) {}

    void reset() const noexcept
    {
        if (*p_) {
            (*f_)(p_);
        }
    }

    A& operator=(const A&) = delete;

    void* create(Size size, const void* caller, struct R& r) const;
    void destroy(void* p, const void* caller, struct R& r) const noexcept;
};

struct R
{
    virtual ~R() = default;
    virtual void* allocate(Size size, const void* caller) = 0;
    virtual void deallocate(void* p, const void* caller) noexcept = 0;
};

R& get_r() noexcept;
void touch(const void* p) noexcept;

struct B
{
    void* q = nullptr;

    static void cb(void** pp) noexcept
    {
        B* self = reinterpret_cast<B*>(pp);
        if (self->q) {
            extern void release_q(void*);
            release_q(self->q);
        }
        self->q = nullptr;
    }

    ~B() noexcept
    {
        touch(this);
        if (q) {
            extern void release_q(void*);
            release_q(q);
        }
    }

    B() noexcept = default;
    B(const B&) = delete;
    B& operator=(const B&) = delete;
    B(B&& other) noexcept : q(other.q)
    {
        other.q = nullptr;
    }
    B& operator=(B&& other) noexcept
    {
        if (this != &other) {
            if (q) {
                extern void release_q(void*);
                release_q(q);
            }
            q = other.q;
            other.q = nullptr;
        }
        return *this;
    }

    operator A() noexcept
    {
        return A(&q, &cb, alignof(void*));
    }
};

void* operator new(Size size, A const& a, R& r) noexcept(false);
void  operator delete(void* p, A const& a, R& r) noexcept;
