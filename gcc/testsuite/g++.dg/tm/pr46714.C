// { dg-do compile }
// { dg-options "-fgnu-tm -O" }

static int asdf __attribute__ ((__weakref__("funky")));

class Building
{
public:
  __attribute__((transaction_safe)) ~Building(void);
};

Building::~Building()
{
}
