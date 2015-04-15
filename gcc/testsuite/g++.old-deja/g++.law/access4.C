// { dg-do assemble  }
// GROUPS passed access
// (Message bugs/access:3)
// From: jamshid@ses.com (Jamshid Afshar)
// Date:     Wed, 2 Mar 94 18:24:22 CST
// Subject:  g++ 2.5.5 doesn't warn about inaccessible virtual base ctor
// Message-ID: <9403030024.AA04534@ses.com>

class ForceLeafSterile { // { dg-message "" } 
    friend class Sterile;
      ForceLeafSterile() {} // { dg-message "" } 
};

class Sterile : private virtual ForceLeafSterile {
public:
    Sterile() {}
    Sterile(const char* /*blah*/) {}
};

class Illegitimate : public Sterile {
public:
    Illegitimate() {}           // { dg-error "" }  can't access virtual base deflt ctor
    Illegitimate(const char* /*blah*/)
        : ForceLeafSterile() {} // { dg-error "" } can't access default ctor
    Illegitimate(const Illegitimate&)
        {}                      // { dg-error "" } can't access default ctor
};
