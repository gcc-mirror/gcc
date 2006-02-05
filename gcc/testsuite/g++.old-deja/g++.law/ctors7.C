// { dg-do assemble  }
// GROUPS passed constructors
// ctors file
// Message-Id: <3JUL199214462623@envmsa.eas.asu.edu>
// From: peckham@envmsa.eas.asu.edu (Bill)
// Subject: const constructor causes crash
// Date: Fri, 3 Jul 1992 21:46:00 GMT

class base_t {
public:
   virtual const char* name () { return "base_t"; }

   base_t ();
   virtual ~base_t ();
};

class d1_t : public base_t {
public:
   virtual const char* name () { return "d1_t"; }

   //   The const keyword on this constructor is an error,  but  it shouldn't
   // cause the compiler to crash.

   d1_t () const;// { dg-error "" } .*const.*
   virtual ~d1_t ();
};

class d2_t : public base_t {
public:
   virtual const char* name () { return "d2_t"; }

   d2_t (int);
   virtual ~d2_t ();
};

