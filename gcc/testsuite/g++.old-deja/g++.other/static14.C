// Build don't run:
// Origin: scott snyder <snyder@fnal.gov>

struct basic_string
{
  ~basic_string();
};

struct Side
{
  void name()
  {
    static basic_string sname;
  }
};

int main ()
{
}
