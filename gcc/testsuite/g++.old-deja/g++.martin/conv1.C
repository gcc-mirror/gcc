// excess errors test - XFAIL
struct S{
  operator bool()
  {
    return true;
  }
};

int main()
{
  S a;
  if (S &b = a);
}

