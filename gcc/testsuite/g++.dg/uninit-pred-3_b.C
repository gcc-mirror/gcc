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
bool get_url3 (A *);

class M {

 public:
 __attribute__ ((always_inline))
 bool GetC (int *c)  {

    A details_str;

    /* Initialization path 1  */
    if (get_url (&details_str))
      {
        *c = get_time ();
        return true;
      }

    /* Destructor call before return*/
    A tmp_str;

    /* Initialization path 2  */
    if (get_url2 (&details_str))
      {
        *c = get_time ();
        return true;
      }

    /* Fail to initialize in this path but
       still returns true  */
    if (get_url2 (&details_str))
      {
        /* Fail to initialize *c  */
        return true;
      }

    return false;
  }

  void do_sth();
  void do_sth2();

  void P (int64 t)
    {
      int cc;
      if (!GetC (&cc))
        return;

      if (cc <= 0)   /* { dg-warning "uninitialized" "uninitialized variable warning" } */
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
