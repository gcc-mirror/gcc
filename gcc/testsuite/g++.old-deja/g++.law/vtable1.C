// Build don't link:
// Special g++ Options: -w
// GROUPS passed vtable
// vtable file
// From: mrs@cygnus.com (Mike Stump)
// Date:     Wed, 20 Apr 1994 17:46:11 -0700
// Subject:  vtable name generation is wrong
// Message-ID: <199404210046.RAA25652@rtl.cygnus.com>

// prepare_fresh_vtable doesn't build the names of
// vtables very well.

struct B {
  virtual void vf() { }
};

struct Main {
  virtual void vf() { }
};

struct Other : public Main, public B {
  virtual void vf() { }
};

struct D : public Main, public B, public Other {
  virtual void vf() { }
} a;
