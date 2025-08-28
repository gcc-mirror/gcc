// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontracts-client-check=none" }
static int pre_check = 0;
bool fpre()
{
  pre_check++;
  return true;
}

static int post_check = 0;
bool fpost()
{
  post_check++;
  return true;
}


int f(const int a, const int b) pre (fpre()) post(fpost()){ return b;  }


struct S
{
  int f(const int a, const int b) post(fpost()){ return b;  }
};

template<typename T>
struct TS
{
  int f(const int a, const T b) pre (fpre()) { return b;  }

  template <typename U>
  int tf(const int a, const U b) pre (fpre()) post(fpost()){ return b;  }
};

int main(int, char**)
{
  f(1,1);
  contract_assert(pre_check == 1);
  contract_assert(post_check == 1);

  pre_check = 0;
  post_check = 0;

  S s;
  s.f(1,1);
  contract_assert(post_check == 1);

  pre_check = 0;
  post_check = 0;

  TS<int> ts;
  ts.f(1,1);
  contract_assert(pre_check == 1);

  pre_check = 0;
  post_check = 0;

  ts.tf(1,1);
  contract_assert(pre_check == 1);
  contract_assert(post_check == 1);


  return 0;
}
