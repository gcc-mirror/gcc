// PR ipa/82352
// { dg-do compile }
// { dg-options "-O2" }

typedef __SIZE_TYPE__ size_t;

class A
{
public :
  typedef enum { Zero = 0, One = 1 } tA;
  A(tA a) { m_a = a; }

private :
  tA m_a;
};

class B
{
public :
  void *operator new(size_t t) { return (void*)(42); };
};

class C
{
public:
  virtual void ffff () = 0;
};

class D
{
 public :
  virtual void g() = 0;
  virtual void h() = 0;
};

template<class T> class IIII: public T, public D
{
public:
 void ffff()
 {
   if (!m_i2) throw A(A::One);
 };

 void h()
 {
  if (m_i2) throw A(A::Zero);
 }

protected:
 virtual void g()
 {
  if (m_i1 !=0) throw A(A::Zero);
 };

private :
 int m_i1;
 void *m_i2;
};

class E
{
private:
    size_t m_e;
    static const size_t Max;

public:
    E& i(size_t a, size_t b, size_t c)
    {
        if ((a > Max) || (c > Max)) throw A(A::Zero );
        if (a + b > m_e) throw A(A::One );
        return (*this);
    }

  inline E& j(const E &s)
    {
      return i(0,0,s.m_e);
    }
};

class F : public C { };
class G : public C { };
class HHHH : public B, public F, public G { };

void k()
{
    new IIII<HHHH>();
}

void l()
{
  E e1, e2;
  e1.j(e2);
}
