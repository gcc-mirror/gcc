// { dg-do assemble  }
// 981203 bkoz
// g++/13478
  
class A {};
class AData {};

typedef void (A::* hand) (void);

struct hand_table {
  const int data1;
  const hand data2;
};

class Agent : public A {
public:
  enum { first = 1, last };
protected:
  static const hand_table table_1[];
  static const AData 	  table_2;
private:
  void foo (void);
};

const hand_table Agent::table_1[] = 
{
   {0,     &Agent::table_2},
   {first, &Agent::foo},
   {last,  &(hand)Agent::foo} // { dg-error "" } no match
};
