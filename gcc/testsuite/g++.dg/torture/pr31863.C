/* { dg-do link } */
/* { dg-timeout-factor 2.0 } */

namespace Loki
{
    class NullType {};
    template <class T, class U>
    struct Typelist
    {
       typedef T Head;
       typedef U Tail;
    };



    namespace TL
    {
        template
        <
                typename T1 = NullType, typename T2 = NullType, typename T3 =
NullType,
                typename T4 = NullType, typename T5 = NullType, typename T6 =
NullType,
                typename T7 = NullType, typename T8 = NullType, typename T9 =
NullType,
                typename T10 = NullType, typename T11 = NullType, typename T12
= NullType,
                typename T13 = NullType, typename T14 = NullType, typename T15
= NullType,
                typename T16 = NullType, typename T17 = NullType, typename T18
= NullType,
                typename T19 = NullType, typename T20 = NullType, typename T21
= NullType,
                typename T22 = NullType, typename T23 = NullType, typename T24
= NullType,
                typename T25 = NullType, typename T26 = NullType, typename T27
= NullType,
                typename T28 = NullType, typename T29 = NullType, typename T30
= NullType,
                typename T31 = NullType, typename T32 = NullType, typename T33
= NullType,
                typename T34 = NullType, typename T35 = NullType, typename T36
= NullType,
                typename T37 = NullType, typename T38 = NullType, typename T39
= NullType,
                typename T40 = NullType
        >
        struct MakeTypelist
        {
        private:
            typedef typename MakeTypelist
            <
                T2 , T3 , T4 ,
                T5 , T6 , T7 ,
                T8 , T9 , T10,
                T11, T12, T13,
                T14, T15, T16,
                T17, T18, T19,
                T20, T21, T22,
                T23, T24, T25,
                T26, T27, T28,
                T29, T30, T31,
                T32, T33, T34,
                T35, T36, T37,
                T38, T39, T40
            >
            ::Result TailResult;

        public:
            typedef Typelist<T1, TailResult> Result;
        };

        template<>
        struct MakeTypelist<>
        {
            typedef NullType Result;
        };

    }
}
template <class Key>
class Factory;

template <class Key, bool iW>
struct Context
{
    typedef Key KeyType;
    enum
    {
        isWrite = iW
    };
};

namespace detail
{

template <class Key, bool isWrite>
class CreatorUnitBaseImpl
{
public:
    typedef Context<Key, isWrite> Context_;
private:
    typedef void*(CreatorUnitBaseImpl::*CreateFun)(Context_&, unsigned&, const
Key&);
    CreateFun createFun_;

protected:
    virtual void* createUninitialized () = 0;
    template <class Value>
    void* createImpl (Context_& ctx, unsigned& ver, const Key& k)
    {
        return createUninitialized();
    }
private:
    CreatorUnitBaseImpl();
public:
    template <class Value>
    CreatorUnitBaseImpl (Value*) :
        createFun_( &CreatorUnitBaseImpl::template createImpl<Value> )
    {
    }

    virtual ~CreatorUnitBaseImpl () {}

    CreatorUnitBaseImpl(const CreatorUnitBaseImpl& s)
        : createFun_(s.createFun_)
    {
    }

    CreatorUnitBaseImpl& operator=(const CreatorUnitBaseImpl& s)
    {
        createFun_ = s.createFun_;
        return *this;
    }
    void* create (Context_& ctx, unsigned& ver, const Key& k)
    {
        return (this->*createFun_)(ctx, ver, k);
    }
};

template <class Key>
class Creator : protected CreatorUnitBaseImpl<Key, true>, protected
CreatorUnitBaseImpl<Key, false>
{
public:
    typedef void* (*CreatorFun) ();

private:
    CreatorFun fun_;
protected:
    virtual void* createUninitialized ()
    {
        if (fun_)
            return (*fun_)();
        return 0;
    }
private:
    Creator ();
public:
    template <class Value>
    Creator (CreatorFun f, Value*) :
        CreatorUnitBaseImpl<Key, true>((Value*)0),
        CreatorUnitBaseImpl<Key, false>((Value*)0),
        fun_(f)
    {
    }

    Creator(const Creator& s) :
        CreatorUnitBaseImpl<Key, true>(s),
        CreatorUnitBaseImpl<Key, false>(s),
        fun_(s.fun_)
    {

    }

    Creator& operator=(const Creator& s)
    {
        CreatorUnitBaseImpl<Key, true>::operator=(s);
        CreatorUnitBaseImpl<Key, false>::operator=(s);
        fun_ = s.fun_;
        return *this;
    }

    virtual ~Creator ()
    {
    }

    template <class Context>
    void* createObject (Context& ctx, unsigned& ver, const Key& k)
    {
        void* r = CreatorUnitBaseImpl<Key, Context::isWrite>::create(ctx, ver,
k);
        return r;
    }
};

}

template <class Key>
class Factory
{
public:
    typedef Key KeyType;
    typedef void* (*CreatorFun) ();
    typedef detail::Creator<Key> Creator;
public:
    Factory () {}
    ~Factory () {}

    template <class Value>
    bool registerCreator (const Key& k, CreatorFun fun)
    {
        return true;
    }
    template <class Context>
    void* createObject (const Key& k, Context& ctx, unsigned& ver)
    {
        return 0;
    }
};

template <class Key, class Base, Key key>
struct ClassSpec
{
    typedef Key KeyType;
    typedef Base BaseType;
    enum {KeyValue = key};
};

template <class Key, class T>
class Serializer;

template <class Key, class Base, Key key>
class Serializer<Key, ClassSpec <Key, Base, key> >
    : public virtual Factory<Key>
{
    typedef Key KeyType;
    typedef Base BaseType;
    enum {KeyValue = key};
    typedef Factory<Key> Inherited;
    typedef Serializer<Key, ClassSpec< Key, Base, key > > SelfType;

    static void* create ()
    {
        return (void*) (new BaseType);
    }
public:
    Serializer()
    {
        Inherited::template registerCreator<BaseType>(
                KeyValue,
                &SelfType::create);
    }
};

template <class Key, class Head>
class Serializer<Key, Loki::Typelist<Head, Loki::NullType> >:
    public Serializer<Key, Head>
{
};

template <class Key, class Head, class Tail>
class Serializer<Key, Loki::Typelist<Head, Tail> >:
    public virtual Serializer<Key, Head>,
    public virtual Serializer<Key, Tail>
{
};

template <class Key>
class Serializer<Key, Loki::NullType> : public virtual Factory<Key>
{
};




typedef unsigned KeyType;



typedef Factory<KeyType> FactoryType;

typedef KeyType Key;

struct A001
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 1; }
    static const char* className () {return "A001";}
};

struct A002
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 2; }
    static const char* className () {return "A002";}
};

struct A003
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 3; }
    static const char* className () {return "A003";}
};

struct A004
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 4; }
    static const char* className () {return "A004";}
};

struct A005
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 5; }
    static const char* className () {return "A005";}
};

struct A006
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 6; }
    static const char* className () {return "A006";}
};

struct A007
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 7; }
    static const char* className () {return "A007";}
};

struct A008
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 8; }
    static const char* className () {return "A008";}
};

struct A009
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 9; }
    static const char* className () {return "A009";}
};

struct A010
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 10; }
    static const char* className () {return "A010";}
};

struct A011
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 11; }
    static const char* className () {return "A011";}
};

struct A012
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 12; }
    static const char* className () {return "A012";}
};

struct A013
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 13; }
    static const char* className () {return "A013";}
};

struct A014
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 14; }
    static const char* className () {return "A014";}
};

struct A015
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 15; }
    static const char* className () {return "A015";}
};

struct A016
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 16; }
    static const char* className () {return "A016";}
};

struct A017
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 17; }
    static const char* className () {return "A017";}
};

struct A018
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 18; }
    static const char* className () {return "A018";}
};

struct A019
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 19; }
    static const char* className () {return "A019";}
};

struct A020
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 20; }
    static const char* className () {return "A020";}
};

struct A021
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 21; }
    static const char* className () {return "A021";}
};

struct A022
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 22; }
    static const char* className () {return "A022";}
};

struct A023
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 23; }
    static const char* className () {return "A023";}
};

struct A024
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 24; }
    static const char* className () {return "A024";}
};

struct A025
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 25; }
    static const char* className () {return "A025";}
};

struct A026
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 26; }
    static const char* className () {return "A026";}
};

struct A027
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 27; }
    static const char* className () {return "A027";}
};

struct A028
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 28; }
    static const char* className () {return "A028";}
};

struct A029
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 29; }
    static const char* className () {return "A029";}
};

struct A030
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 30; }
    static const char* className () {return "A030";}
};

struct A031
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 31; }
    static const char* className () {return "A031";}
};

struct A032
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 32; }
    static const char* className () {return "A032";}
};

struct A033
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 33; }
    static const char* className () {return "A033";}
};

struct A034
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 34; }
    static const char* className () {return "A034";}
};

struct A035
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 35; }
    static const char* className () {return "A035";}
};

struct A036
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 36; }
    static const char* className () {return "A036";}
};

struct A037
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 37; }
    static const char* className () {return "A037";}
};

struct A038
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 38; }
    static const char* className () {return "A038";}
};

struct A039
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 39; }
    static const char* className () {return "A039";}
};

struct A040
{
    template <class Context>
    bool serialize(Context& ctx, unsigned& ver)
    {
        return true;
    }
    static Key classId() { return 40; }
    static const char* className () {return "A040";}
};

Factory<Key>& getInstance()
{
    static Serializer<Key,
        Loki::TL::MakeTypelist<
            ClassSpec<Key, A001, 1>,
            ClassSpec<Key, A002, 2>,
            ClassSpec<Key, A003, 3>,
            ClassSpec<Key, A004, 4>,
            ClassSpec<Key, A005, 5>,
            ClassSpec<Key, A006, 6>,
            ClassSpec<Key, A007, 7>,
            ClassSpec<Key, A008, 8>,
            ClassSpec<Key, A009, 9>,
            ClassSpec<Key, A010, 10>,
            ClassSpec<Key, A011, 11>,
            ClassSpec<Key, A012, 12>,
            ClassSpec<Key, A013, 13>,
            ClassSpec<Key, A014, 14>,
            ClassSpec<Key, A015, 15>,
            ClassSpec<Key, A016, 16>,
            ClassSpec<Key, A017, 17>,
            ClassSpec<Key, A018, 18>,
            ClassSpec<Key, A019, 19>,
            ClassSpec<Key, A020, 20>,
            ClassSpec<Key, A021, 21>,
            ClassSpec<Key, A022, 22>,
            ClassSpec<Key, A023, 23>,
            ClassSpec<Key, A024, 24>,
            ClassSpec<Key, A025, 25>,
            ClassSpec<Key, A026, 26>,
            ClassSpec<Key, A027, 27>,
            ClassSpec<Key, A028, 28>,
            ClassSpec<Key, A029, 29>,
            ClassSpec<Key, A030, 30>,
            ClassSpec<Key, A031, 31>,
            ClassSpec<Key, A032, 32>,
            ClassSpec<Key, A033, 33>,
            ClassSpec<Key, A034, 34>,
            ClassSpec<Key, A035, 35>,
            ClassSpec<Key, A036, 36>,
            ClassSpec<Key, A037, 37>,
            ClassSpec<Key, A038, 38>,
            ClassSpec<Key, A039, 39>,
            ClassSpec<Key, A040, 40>
        >::Result
    > instance;
    return instance;
}

int main ()
{
    return 0;
}
