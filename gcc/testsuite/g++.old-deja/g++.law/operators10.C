// Build don't link: 
// GROUPS passed operators
// opr-eq file
// Message-Id: <9306040324.AA22954@balder.cs.wisc.edu>
// From: so@cs.wisc.edu (Bryan So)
// Subject: g++ bug
// Date: Thu, 3 Jun 93 22:24:13 -0500

template <class T>
struct Test {
    int data;
    Test& operator=(int i) { data = i; return *this; }
};


int main()
{
    Test<int> i, j;

    i = j;

    return 0;
}
