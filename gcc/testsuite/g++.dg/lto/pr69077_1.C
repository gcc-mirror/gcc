struct cStdDev
{
  long ns;
  virtual double mean() const {  return ns;  }
};

struct sf
{
  void recordScalar(double value);
  cStdDev eedStats;
  virtual void finish();
};
void sf::finish() {
    recordScalar(eedStats.mean());
}
