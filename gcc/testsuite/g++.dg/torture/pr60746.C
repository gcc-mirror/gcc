// { dg-do compile }

class One
{
public:
  virtual unsigned long getSize () const;
};

class Two
{
  virtual int run ();
};

int
Two::run ()
{
  One list_arry[5][2];
  int orig = 0;
  if (list_arry[3][orig].getSize () > 0
      || list_arry[4][orig].getSize () > 0)
    {
    }

  return 0;
}
