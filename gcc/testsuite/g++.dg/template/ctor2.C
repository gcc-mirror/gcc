// { dg-do run }

int i;

template <class T>
struct S
{
  S () { i = 1; }
};

static S<int> s[1];

int main ()
{
  if (!i)
    return 1;
}

