// Origin: PR c++/42766
// { dg-do compile }

template<class T> class smart_pointer {
public:
    operator T* () const { }
    operator bool () const { }
    operator bool () { }
};
class Context { };
typedef smart_pointer<Context> ContextP;
class SvnClient  {
    ~SvnClient();
    ContextP svnContext;
};
SvnClient::~SvnClient() {
    delete svnContext;
}
