// PR c++/44778

enum Healpix_Ordering_Scheme { RING, NEST };

class Healpix_Base
  {
  protected:
    Healpix_Ordering_Scheme scheme_;
    int nest2ring (int pix) const;
    int ring2nest (int pix) const;

    typedef int (Healpix_Base::*swapfunc)(int pix) const;
  };

template<typename T> class Healpix_Map: public Healpix_Base
  {
  public:
    void Import_nograde (const Healpix_Map<T> &orig)
      {
        swapfunc swapper = (scheme_ == NEST) ?
          &Healpix_Map::ring2nest : &Healpix_Map::nest2ring;
      }
  };

int main()
  {
  Healpix_Map<double> a,b;
  a.Import_nograde(b);
  }
