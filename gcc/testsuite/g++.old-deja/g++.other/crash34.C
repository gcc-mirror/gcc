// { dg-do assemble }
// Origin: Jakub Jelinek <jakub@redhat.com>


class v
{
    double x, y;
public:
    v();
};

class w : public v {
public :
    static const w X;
    w();
};

void bar(w x);

void foo()
{
  bar(w::X);
}
