// Build don't link:

class NF { };

struct QF {
  NF nf;

  typedef float(NF::* const NPF)() const;

  void p (NPF npf) const {
    float q = (nf.*npf)();
  }
};
