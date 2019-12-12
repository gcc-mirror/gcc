// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -O3 -g -flto } } }
// { dg-extra-ld-options "-r -nostdlib" }

struct cStdDev
{
  long ns;
  virtual double mean() const {  return ns;  }
};

struct cWeightedStdDev : public cStdDev {
    virtual int netPack();
};
int cWeightedStdDev::netPack() { return 0; }
