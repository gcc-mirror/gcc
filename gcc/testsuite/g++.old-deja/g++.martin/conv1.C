// { dg-do run  }
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

