// Special g++ Options: -Wall

#include <list>

template < class N, class A >
class Base
{
public:
    class NN : public N
    {
        friend Base<N,A>;
        friend ostream& operator<<(ostream &os, const NN& n)
        {
            n.print(os);
            return os;
        }
    public:
        void print(ostream &os) const
        {
            os << "Base<N,A>::print()" << endl;
            N::print(os);
        }
    };

    typedef NN*                        NNPtr;
    typedef list<NNPtr>                NList;
    typedef NList::iterator            n_list_iter;

    class n_iter : public n_list_iter 
    {
        friend bool operator<(n_iter a, n_iter b)
        {
            return (a->ind() < b->ind());
        }
        friend bool operator>(n_iter a, n_iter b)
        {
            return (a->ind() > b->ind());
        }

    public:
        n_iter() {}
        n_iter(n_list_iter i) : n_list_iter(i) {}
        
        NN& operator*()
            { return *n_list_iter::operator*();};
        NN* operator->() 
            { return n_list_iter::operator*(); }
    };
private:
    NList    ns;
    n_iter new_n_aux(NN* nd)
    {
        ns.push_back(nd);
        n_iter  nodi = --ns.end();
        return (nodi);
    }
public:
    n_iter new_n()
    {
        return new_n_aux(new NN());
    }
    void del_n(n_iter itr) 
    {
        NN* n = itr.operator->();
        ns.erase(itr);
        delete n;
    }
    n_iter           beg_n()          {   return (ns.begin()); }
    n_iter           end_n()          {   return (ns.end());   }
};

template <class XN, class XA>
class YN : public XN
{
public:    
    YN() {};
    void print(ostream& os) const
    {
        os << "YN<XN,XA>::print() " << endl;
        XN::print(os);
    }
    friend ostream& operator<< (ostream& os, const YN& wn)
    {
        wn.print(os);
        return os;
    }
};

template <class XN, class XA>
class YA : public XA
{
public:    
    YA() {};
    void print(ostream &os) const
    {
        os << "YA<XN,XA>::print() " << endl;
        XA::print(os);
    }
    
    friend ostream& operator<<(ostream& os, const YA &wa)
    {
        wa.print(os);
        return os;
    }
};


template<class XN, class XA>
class XBase : public Base< YN<XN, XA>, YA<XN, XA> >
{
public:
    typedef     Base< YN<XN,XA>, YA<XN,XA> >    Net;
    typedef     Net::n_iter          n_iter;
    XBase() {};
};


class MyClass
{
public:
  struct ZN
  {
    void print(ostream &os) const
      {
        os << "MyClass::ZN::print()" << endl;
      }
    inline friend ostream& operator<<(ostream& os, const MyClass::ZN& nd)
      {
        nd.print(os);
        return os;
      }
  };
  struct ZA
  {
    void print(ostream& os) const
      {
        os << "MyClass::ZA::print()" << endl;
      }
    inline friend ostream& operator<<(ostream& os, const MyClass::ZA& ar)
      {
        ar.print(os);
        return os;
      }
  };

  typedef XBase<ZN,ZA>                    MyXBase;
  typedef MyXBase::n_iter                 my_n_iter;
  MyXBase                                 xbase;
};

main ()
{
  MyClass mine;
  MyClass::my_n_iter  n1, n2, n3, n4;

  n1 = mine.xbase.new_n();
  n2 = mine.xbase.new_n();
  n3 = mine.xbase.new_n();
  n4 = mine.xbase.new_n();
  
  cout << *n1 << endl;
  cout << *n2 << endl;
  cout << *n3 << endl;
  cout << *n4 << endl;
  
  mine.xbase.del_n(n1);
  mine.xbase.del_n(n2);
  mine.xbase.del_n(n3);
  mine.xbase.del_n(n4);
}



