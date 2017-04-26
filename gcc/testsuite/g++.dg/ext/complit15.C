// PR c++/79580
// { dg-options "-flto -std=c++98" }

class a
{
  static const double b;
};
const double a::b ((union { double c; }){}.c);
