// PR middle-end/25977
// Bug: We were assuming that the return slot of the current function is
// always safe to use as the return slot for another call, because its
// address cannot escape.  But its address can escape if we perform the
// named return value optimization.

// { dg-do run }

struct A
{
    A( int left, int top, int width, int height )
      : x1(left), y1(top), x2(left+width-1), y2(top+height-1) {}

    //A(const A& o) : x1(o.x1), y1(o.y1), x2(o.x2), y2(o.y2) {}
    //A& operator=(const A& o ) { x1=o.x1; y1=o.y1; x2=o.x2; y2=o.y2; return *this; }

    A  operator&(const A &r) const
    {
	A tmp(0, 0, -1, -1);
	tmp.x1 = ((r.x1) < (x1) ? (x1) : (r.x1));
	tmp.x2 = ((x2) < (r.x2) ? (x2) : (r.x2));
	tmp.y1 = ((r.y1) < (y1) ? (y1) : (r.y1));
	tmp.y2 = ((y2) < (r.y2) ? (y2) : (r.y2));
	return tmp;
    }

    int x1;
    int y1;
    int x2;
    int y2;
};

bool operator==( const A &r1, const A &r2 )
{
    return r1.x1==r2.x1 && r1.x2==r2.x2 && r1.y1==r2.y1 && r1.y2==r2.y2;
}

static A test()
{
    A all = A( 0, 0, 1024, 768);
    A a = all;
    A r = all;
    a = a & r;
    return a;
}

extern "C" void abort(void);

int main( int argc, char ** argv )
{
    A all = A( 0, 0, 1024, 768);
    A a = test();

    if ( ! ( a == all))
      abort();

    return 0;
}
