// { dg-do assemble  }
// PRMS Id: 10860
class Beige
{
public:
    static int yellow();
    void white(int green = yellow());
    void aqua(int green = Beige::yellow());
};
