// PR c++/8660
// Bug: we were treating the definition of the non-template as a definition
// of the template, which broke.

struct BadgerBuf
{
  void ReadPod();
  template<class B>
  void ReadPod();
};

void BadgerBuf::ReadPod ()
  { ReadPod<int> (); }
