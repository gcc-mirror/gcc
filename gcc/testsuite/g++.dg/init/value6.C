// PR c++/39109

struct N
{
  private:
    virtual ~N ();
};

N *
foo ()
{
  return new N ();
}
