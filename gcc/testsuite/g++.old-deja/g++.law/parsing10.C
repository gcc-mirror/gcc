// { dg-do assemble  }
// GROUPS passed parsing
// parsing folder
// From: Eirik Fuller <eirik@elf.ithaca.ny.us>
// Date:     Wed, 15 Dec 1993 17:06:11 -0500
// Subject:  parse error
// Message-ID: <199312152206.AA06584@tonttu.TC.Cornell.EDU>


class s;

template <class T>
class t
{
public:
  void f(T *t);
};

class l
{
public:
  void s() {}
};

extern t<l> g;

class p
{
public:
  void *h;
  s *a() {return (s *) h;}
};
