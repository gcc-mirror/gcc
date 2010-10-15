/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

/* Multiple initialization paths.  */

typedef long long int64;
void incr ();
bool is_valid (int);
int  get_time ();

class A
{
public:
  A ();
  ~A () {
    if (I) delete I;
  }

private:
  int* I;
};

bool get_url (A *);
bool get_url2 (A *);

class M {

 public:
 __attribute__ ((always_inline))
 bool GetC (int *c)  {

    A details_str;
    /* Intialization path 1  */
    if (get_url (&details_str))
      {
        *c = get_time ();
        return true;
      }

    /* insert dtor calls (inlined) into following return paths  */
    A tmp_str;

    /* Intializtion path 2  */
    if (get_url2 (&details_str))
      {
        *c = get_time ();
        return true;
      }

    return false;
  }

  void do_sth();
  void do_sth2();

  void P (int64 t)
    {
      int cc;
      if (!GetC (&cc)) /* return flag checked properly */
        return;

      if (cc <= 0)   /* { dg-bogus "uninitialized" "uninitialized variable warning" } */
        {
          this->do_sth();
          return;
        }

    do_sth2();
  }
};

M* m;
void test(int x)
{
  m = new M;
  m->P(x);
}
