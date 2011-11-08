// { dg-do compile }
// { dg-options "-fgnu-tm -O1" }

__attribute__((transaction_safe))
void* operator new (__SIZE_TYPE__);

__attribute__((transaction_pure))
inline int atomic_exchange_and_add( int * pw, int dv )
{
    int r;
    __asm__ ("" : "=r"(r));
    return r;
}

class sp_counted_base
{
protected:
    int use_count_;        // #shared
public:
    __attribute__((transaction_safe))
    virtual void dispose() = 0; // nothrow

    __attribute__((transaction_safe))
    void release() // nothrow
    {
	if( atomic_exchange_and_add( &use_count_, -1 ) == 1 )
	{
	    dispose();
	}
    }
};

class sp_counted_base_x86 : public sp_counted_base
{
public:
  void dispose()
  {
    release();
  }
};

class shared_count
{
private:
    sp_counted_base * pi_;
public:
    int j;
    __attribute__((transaction_safe))
    shared_count(): pi_(new sp_counted_base_x86()), j(0)
    {
    }
    __attribute__((transaction_safe))
    ~shared_count() // nothrow
    {
	if( pi_ != 0 ) pi_->release();
    }
};

volatile int i = 1;
shared_count * c;
int main()
{
  if ( i == 0) {
    __transaction_atomic {
     shared_count sc;
    }
  }
  return 0;
}
