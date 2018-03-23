// { dg-lto-do run }
// { dg-lto-options { { -O3 -flto } } }

class Position
{
  public:
    Position( void ) {}
    virtual ~Position() {}

    virtual void calcPercent( const char *name,int pos,int size ) {}
};


class Looper
{
  public:
    Looper( Position *cc,int size )
      : m_cc(cc), m_size(size) {}
    virtual ~Looper() {}

    void loop( void )
    {
      for( int pos=0; pos<m_size; pos++ )
      {
        m_cc->calcPercent( "",pos,m_size );
      }
    }

  private:
    Position *m_cc;
    int m_size;
};


class EmptyClass
{
  public:
    EmptyClass( void ) {}
    virtual ~EmptyClass() {}
};


class Combined : public EmptyClass, public Position
{
  public:
    Combined( void ) : m_percent(0) {}
    ~Combined() {}

    void calcPercent( const char *name,int pos,int size )
    {
      int percent = 100*pos/size;
      if( percent!=m_percent )
        m_percent = percent;
    }

  private:
    int m_percent;
};



int main( int argc,char **argv )
{
  Combined *comb = new Combined();
  Looper *looper = new Looper( comb,argc );

  looper->loop();

  delete comb;
  delete looper;

  return( 0 );
}
