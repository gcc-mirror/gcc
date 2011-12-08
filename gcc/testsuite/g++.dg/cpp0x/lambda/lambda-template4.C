// PR c++/51459
// { dg-do run { target c++11 } }

struct func {
    virtual ~func() { }
    virtual void operator()() const = 0;
    virtual func* clone() const = 0;
};

template<typename T>
struct funcimpl : func {
    explicit funcimpl(T t) : t(t) { }
    void operator()() const { t(); }
    func* clone() const { return new funcimpl(*this); }
    T t;
};

struct function
{
    func* p;

    template<typename T>
        function(T t) : p(new funcimpl<T>(t)) { }

    ~function() { delete p; }

    function(const function& f) : p(f.p->clone()) { }

    function& operator=(const function& ) = delete;

    void operator()() const { (*p)(); }
};

template <typename F>
function animate(F f) { return [=]{ f(); }; }

int main()
{
  function linear1 = []{};
  function av(animate(linear1));
  av();
}
