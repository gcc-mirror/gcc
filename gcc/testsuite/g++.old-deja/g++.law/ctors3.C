// Build don't link: 
// GROUPS passed constructors
// ctor file
// Message-Id: <m0oxRi1-0002fPC@nesmith.pactitle.com>
// From: genehi@nesmith.pactitle.com (Gene Hightower)
// Subject: Apparent bug in g++ 2.5.2
// Date: Wed, 10 Nov 93 18:24 PST

class One {
  public:
    One() {}
};

template<class T> class Two : public One {
};

class Three {
};

class Four  {
    Two<Three>  x[1];
};

class Five {
    Four        y;

  public:
    Five() {}
};
