// { dg-do assemble  }
// { dg-options "-O" }
// Origin: Thomas Kunert <kunert@physik.tu-dresden.de>

struct C {
    ~C();
};    

struct R {
    bool empty() const;
    C m_;
};

struct R1 {
    R1( const R& a );
  ~R1 ();
    C m_;
};

R1 get_empty();

R1::R1( const R& a ) :
    m_( a.empty() ? get_empty().m_ : C() )
{}

void qnorm( const R & r)
{ R1 n( r ); } 
