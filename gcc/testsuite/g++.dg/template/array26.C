// PR c++/57325

class valarray { int _M_data; };
template < typename > struct SimpleJet { valarray partials; };

template < class C > struct scoped_ptr_impl
{
    scoped_ptr_impl (C *):data_ () { }
    struct Data
    {
        C ptr;
    };
    Data data_;
};

template < class, class = int >struct scoped_ptr;
template < class C, class D > struct scoped_ptr <C[], D >
{
    scoped_ptr ():impl_ (0) { }
    scoped_ptr_impl < C > impl_;
};

template < typename JetsT > void
TestJets (JetsT *)
{
    typedef typename JetsT::JetType JetT;
    scoped_ptr < JetT[] > a;
}

template < typename T > struct SimpleJets
{
    typedef SimpleJet < T > JetType;
    scoped_ptr < SimpleJet < T >[] > vars_;
};

void fn ()
{
    SimpleJets < double >b;
    TestJets (&b);
}
