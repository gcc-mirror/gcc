// GROUPS passed conversions
// Build don't link:
typedef unsigned long Array[3];

void sample(const unsigned long (&an_array)[3]);

class Sample
  {
 public:
  void simple(const Array &an_array);
  static void sample(const Array &an_array);
  };

class A
  {
 public:
  Array array;
  };

  Sample s;

  void simple(const A &a)
    {
    s.simple(a.array);
    sample(a.array);
    Sample::sample(a.array);
    }
