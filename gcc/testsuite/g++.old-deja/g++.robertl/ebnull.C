// { dg-do run  }
class null {
    null (null const&);
    void operator& ();

  public:
    null () {}

    template <typename T>
    operator T* () const { return 0; }
} const null;

int main ()
{
    int *p = null;

    return 0;
}
