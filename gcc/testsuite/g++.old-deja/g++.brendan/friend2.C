// Build don't link: 
// GROUPS passed friends
class X {
  int a;
friend void friend_set (X*, int);
};

void friend_set (X *p, int i) { p->a = i; }

void f()
{
  X obj;
  friend_set (&obj, 10);
}

