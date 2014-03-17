// PR ipa/60457
// { dg-do compile }

template <class T>
struct A
{
};

struct B : A <B>
{
  B ();
};

B::B ()
{
  const int c[] = { 1, 1 };
}
