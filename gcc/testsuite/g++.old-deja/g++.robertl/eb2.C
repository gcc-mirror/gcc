// Build don't link: 
typedef __SIZE_TYPE__ size_t;
inline void *operator new(size_t, void *place) throw() { return place; }

struct A
{
    A();
    ~A();
};

void testfunc( void )
{
   A*    mybuf;
   A        v[1];

   new (mybuf) A();
}
