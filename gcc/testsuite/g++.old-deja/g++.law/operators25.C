// { dg-do assemble  }
// GROUPS passed operators
// opr-new file
// From: gparker@world.std.com (Glenn P Parker)
// Date:     Mon, 25 Jan 1993 20:43:43 -0500
// Subject:  gcc bug
// Message-ID: <199301260143.AA14133@world.std.com>

typedef int (**PPF)(int);

int main(void)
{
  PPF pf2 = new (int (*) (int));     // internal compiler error on this line.

  return 0;
}
