int i;

struct CC
{
  virtual ~CC () { ++i; }
};

class BB : virtual public CC
{
};

class AA : public virtual BB
{
};

int main ()
{
  {
    AA xx;
  }
  if (i != 1)
    return 1;
}
