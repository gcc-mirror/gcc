// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <9307152250.AA24812@volterra>
// From: rst@ai.mit.edu (Robert S. Thau)
// Subject: g++ won't convert char[] to char*&
// Date: Thu, 15 Jul 93 18:50:59 EDT


// Compiles fine with Sun CC 2.1

void f(const char *& x) // { dg-message "argument" }
{
  x++;
}

int main()
{
  f ("foo"); // { dg-error "invalid initialization" }
}
