// Build don't link:

class S 
{
  ~S();
public:
  friend void f();
};


S::~S()
{
  static S s;
}
