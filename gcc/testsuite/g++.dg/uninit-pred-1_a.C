/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

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

class M {

 public:
__attribute__ ((always_inline))  int GetC (int *c)  {

    A details_str;
    if (!get_url (&details_str))
      {
        incr ();
        return 1;
      }

    *c = get_time ();
    return -1;
  }

  void do_sth();
  void do_sth2();
   
  void P (int64 t)
    {
      int cc; /* { dg-bogus "uninitialized" "uninitialized variable warning" }  */ 
      if (GetC (&cc) >= 0 )
        return;
      
      if (t && cc <= 0 )  /* { dg-bogus "uninitialized" "uninitialized variable warning" } */
        {
          this->do_sth();
          return;
        }

    do_sth2();
  }
};

M* m; 
void foo(int x)
{
  m = new M;
  m->P(x);
}
