// { dg-do assemble  }
// { dg-options "-fexceptions" }
// GROUPS passed exceptions
// except file
// Message-Id: <9211301118.AA09810@ss670mp.geco.slb.com>
// From: willoch@ss670mp.oslo.sgp.slb.com (thorbjorn willoch)
// Subject: -fansi-exceptions bug
// Date: Mon, 30 Nov 92 11:18:05 GMT

extern "C" int printf(const char *, ...);

class Vector
{
    int* p;
    int sz;
  public:
    Vector(int s) { p = new int[sz=s]; }
    ~Vector() {delete [] p; }
    int size() {return sz; }
    class Range{};
 
 
    int& operator[](int i);
};
 
int& Vector::operator[](int i)
{
    if(0<=i && i<sz) return p[i];
    throw Range();
}
 
void do_something(Vector& v)
{
    int i = v[v.size()+10];
}
 
main()
{
    Vector v(10);
 
    try
    {
        do_something(v);
    }
 
    catch (Vector::Range)
    {
        printf("Range error exception\n");
    }
}
