// PR tree-optimization/27548
// { dg-do compile }
// { dg-options "-O1" }
// { dg-additional-options "-Wno-return-type" }

namespace Gambit
{
  template < class T > class Array
  {
  protected:int mindex, maxdex;
    T *data;
    int InsertAt (const T & t, int n)
    {
      T *new_data = new T[++this->maxdex - this->mindex + 1] - this->mindex;
      int i;
      for (i = this->mindex; i <= n - 1; i++)
	  new_data[i] = this->data[i];
	new_data[i++] = t;
    }
  public:   Array (unsigned int len = 0):mindex (1), maxdex (len),
      data ((len) ? new T[len] -
	    1 : 0)
    {
    }
    virtual ~ Array ()
    {
      if (maxdex >= mindex)
	delete[](data + mindex);
    }
    const T & operator[] (int index) const
    {
    }
    int Append (const T & t)
    {
      return InsertAt (t, this->maxdex + 1);
    }
  };
}

class gIndexOdometer
{
private:Gambit::Array < int >MinIndices;
    Gambit::Array < int >CurIndices;
    gIndexOdometer (const Gambit::Array < int >, const Gambit::Array < int >);
  void SetIndex (const int &, const int &);
  int NoIndices () const;
  gIndexOdometer AfterExcisionOf (int &) const;
};
gIndexOdometer
gIndexOdometer::AfterExcisionOf (int &to_be_zapped) const
{
  Gambit::Array < int >NewMins, NewMaxs;
  int i;
  for (i = 1; i <= NoIndices (); i++)
    {
      NewMins.Append (MinIndices[i]);
    }
  gIndexOdometer NewOdo (NewMins, NewMaxs);
  for (i = 1; i < to_be_zapped; i++)
    NewOdo.SetIndex (i, CurIndices[i]);
}
