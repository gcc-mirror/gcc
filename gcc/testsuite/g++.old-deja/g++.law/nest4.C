// { dg-do assemble  }
// GROUPS passed nest
// nest file
// From: Neal Young <ney@princeton.edu>
// Date:     Mon, 11 Oct 93 17:03:59 EDT
// Subject:  g++ 2.4.5 bug report: local class decl can't access local static var
// Message-ID: <9310112103.AA06326@cs>

void f()
{
  static int s;

  struct local {
    int j() { return s; }       // should be okay, see 1991 ref. man. r.9.8
  };
}

int main()
{
}
