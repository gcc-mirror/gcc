// Build don't link: 
// Special g++ Options: -Wshadow
// GROUPS passed shadow-warnings
// (Message bugs/shadow:2)
// From: michael@utex.rni.sub.org (Michael Utech)
// Date:     Sat, 22 Jan 1994 04:28:00 +0100
// Subject:  very minor problem/bug in gcc-2.5.4, -Wshadow
// Message-ID: <m0pNZ1T-0008QUC@utex.rni.sub.org>

class X
{
  int count;
public:
  X() {} // necessary to produce the `count' warning
};

template <class T>
class Y
{
  T t;
public:
  int f (int count) { return (count); }
};

main ()
{
  Y<char> y;
}
