// { dg-do assemble  }
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <199301260140.AA13734@world.std.com>
// From: gparker@world.std.com (Glenn P Parker)
// Subject: gcc bug
// Date: Mon, 25 Jan 1993 20:40:44 -0500

int f() {  return 1; }

int main()
{
  int (&fr)() = f;

  return 0;
}
