// { dg-do compile }
// { dg-options "-fgnu-tm -O1" }

__attribute__((transaction_pure))
inline int atomic_exchange_and_add(int dv )
{
    int r;
    __asm__ ("" : "=r"(r));
    return r;
}

class sp_counted_base
{
public:
    __attribute__((transaction_safe))
    void release()
    {
	if( atomic_exchange_and_add(-1 ) == 1 )
	{
	}
    }
};

sp_counted_base *base;

void here(){
  __transaction_atomic {
    base->release();
  }
}
