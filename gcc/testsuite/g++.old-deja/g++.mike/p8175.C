// { dg-do assemble  }
// prms-id: 8175

class AtoBoolC {
public:
  AtoBoolC(const AtoBoolC& aBool);
private:
  int myValue;
};

struct TestCase {
  AtoBoolC is_call_on_unack;
};

static TestCase the_cases[] = { { 0 } };	// { dg-error "" } 
