// { dg-do assemble  }
// GROUPS passed old-abort
//
// This one creates
//
// gcc2: Internal compiler error: program cc1plus got fatal signal 11
//
// when compiled with g++.
// The error goes away, if
// 1) int ClassInvariant() is not virtual or
// 2) GnObject has a virtual destructor or
// 3) GnWidget has no virtual destructor or
// 4) GnContracts has a virtual destructor
//


class GnContracts {
  public:
    virtual int ClassInvariant();
//    virtual ~GnContracts();
};

class GnObject : public GnContracts {
  public:
//    virtual ~GnObject();
};

class GnWidget : public GnObject {
  public:
    virtual ~GnWidget();
};

class GnOptionGroup : public GnObject, public GnWidget {// { dg-warning "inaccessible" }
};

