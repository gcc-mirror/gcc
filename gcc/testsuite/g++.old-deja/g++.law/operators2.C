// { dg-do assemble  }
// GROUPS passed operators
// opr-conv file
// Message-Id: <199301260142.AA13995@world.std.com>
// From: gparker@world.std.com (Glenn P Parker)
// Subject: gcc bug
// Date: Mon, 25 Jan 1993 20:42:35 -0500

int main(void)
{
  int i = int();  // g++ 2.3.3 cannot compile it.

  return 0;
}

