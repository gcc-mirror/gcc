// PRMS Id: 7162
// Build don't link:

struct B {
   int i;
   B() : i(1) {}
};

struct D : B {
   int i;
   D() : i(2) {}
};

void ch()
{
   D od2;
   D &rd2 = od2;

   B &rd1 = dynamic_cast<B&>(rd2);
}   
