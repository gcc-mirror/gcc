// { dg-do run  }
// GROUPS passed operators
// opr-del file
// Message-Id: <199301272137.AA25213@world.std.com>
// From: kol@world.std.com (Nikolay Yatsenko)
// Subject: bug report
// Date: Wed, 27 Jan 1993 16:37:30 -0500

extern "C" int printf(const char* ...);

int     delete_counter = -1;

struct T{
  void operator delete (void * p) {delete_counter ++; ::operator delete(p);}
};

int main(void)
{
  T * ps1 = new T;

  ::delete ps1;             // Wrong T::operator delete() is called here

  if (delete_counter != -1)
    { printf ("FAIL\n"); return 1; }
  else
    printf ("PASS\n");
  return 0;
}
