class JunkBase
{
public:
    virtual void DoSomething( void ) = 0;
protected:
    virtual ~JunkBase( void ) {};
    JunkBase( void ) {}
};

class Junk : protected JunkBase
{
public:
    Junk( void ) : JunkBase() {}
    virtual ~Junk( void ) {}
protected:
    inline JunkBase * AsBase( void )
    { return dynamic_cast< JunkBase * >( this ); }
    virtual void DoSomething( void ) { }
};




