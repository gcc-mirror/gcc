// { dg-do assemble  }
// GROUPS passed unsorted
// code-gen file
// From: klaus@steinitz.mathematik.uni-dortmund.de
// Date:     Mon, 15 Nov 1993 16:51:11 +0100
// Message-ID: <9311151551.AA17761@steinitz.mathematik.uni-dortmund.de>

template <int A,int B>
class X
{
};

template <int A,int B,int C>
X<A,C> f(X<A,B>,X<B,C>)
{
  X<A,C> result;
  return result;
}

int main()
{
  X<1,3> x;
  X<1,2> y;
  X<2,3> z;
  x=f(y,z);
}
