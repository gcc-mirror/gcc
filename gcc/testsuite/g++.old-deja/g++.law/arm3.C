// { dg-do assemble  }
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <199301260139.AA13555@world.std.com>
// From: gparker@world.std.com (Glenn P Parker)
// Subject: gcc bug
// Date: Mon, 25 Jan 1993 20:39:19 -0500

class X {
        enum S { blue, pink };
        int S;
public:
        void f (enum S arg) ;
};

void X::f (enum S arg)
{
        S = arg;        // g++ gives error on this line.
}

