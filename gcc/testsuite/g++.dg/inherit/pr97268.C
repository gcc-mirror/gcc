// { dg-do compile { target c++11 } }
// { dg-additional-options -Wall }
// PR 97268, ICE due to broken inherited-from-virtual base-ctor
class Handle {
public:
    explicit Handle(char const *const &) { }
    ~Handle() {}
    Handle(const Handle &) = delete;
    Handle &operator=(const Handle &) = delete;

protected:
    int lasterr = 0;

};

struct ObjectBase {
    ~ObjectBase() {}

protected:
    explicit ObjectBase(const char *lc_, int ln_, Handle &h, unsigned) 
            :  handle(h) { }

protected:

    Handle &handle;
};

template <bool CACHED>
struct Object : virtual public ObjectBase {
    explicit Object(const char *lc_, int ln_, Handle &env);

protected:
    using ObjectBase::ObjectBase;

};

class BetterObjectBase : virtual public ObjectBase {
protected:
    BetterObjectBase(const char *lc_, int ln_, Handle &env)
            : ObjectBase("", 0, env, 0) {}

};

template <bool CACHED>
class BetterObject : public Object<CACHED>, public BetterObjectBase {
public:
    BetterObject(Handle &env)
      : ObjectBase("", 0, env, 0)
      , Object<CACHED>("", 0, env, 0)
      , BetterObjectBase("", 0, env) {} // { dg-error "use of deleted function" }

};

int main() {
    Handle h("handle");

    BetterObject<true> B(h);

    return 0;
}
