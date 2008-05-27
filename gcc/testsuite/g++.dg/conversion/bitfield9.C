// PR c++/35909
// { dg-do compile }

struct MidiCommand
{
  unsigned data1 : 8;
};

void g(const unsigned char &);
void h(const unsigned int &);

void f(MidiCommand mc)
{
  g(mc.data1);
  h(mc.data1);
}

