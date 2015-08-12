// PR c++/53330
// { dg-do run }

extern "C" void abort ();

struct constr_empty
{
    constr_empty() {};
};

struct noconstr_empty
{
};

struct constr_nonempty
{
    constr_nonempty() {};
    int dummy;
};

struct noconstr_nonempty
{
    int dummy;
};

int main()
{
    volatile constr_empty      *ce = new constr_empty[0];
    volatile noconstr_empty    *ne = new noconstr_empty[0];
    volatile constr_nonempty   *cn = new constr_nonempty[0];
    volatile noconstr_nonempty *nn = new noconstr_nonempty[0];
    volatile int               *ii = new int[0];

    delete [] ce;
    delete [] ne;
    delete [] cn;
    delete [] nn;
    delete [] ii;

    if (!(ce && ne && cn && nn && ii))
      abort ();
}
