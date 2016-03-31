// PR c++/70096
// { dg-do run }

int data_read;

struct Holder
{
  void foo () { data_read = data; }
  int data;
};

void
poison_stack ()
{
  volatile char a[256];
  __builtin_memset ((void *)a, 0xa, sizeof a);
}

template <typename F>
void test1 ()
{
  Holder h;
  h.data = 42;
  F Holder::*fptr = &Holder::foo;
  (h.*fptr)();
}

template <typename F>
void test2 ()
{
  Holder h;
  h.data = 42;
  F Holder::*fptr1 = &Holder::foo;
  F Holder::*fptr2 = fptr1;
  (h.*fptr2)();
}


int main ()
{
  poison_stack ();
  test1<void()>();
  poison_stack ();
  test2<void()>();
}
