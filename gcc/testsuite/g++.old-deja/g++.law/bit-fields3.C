// { dg-do assemble  }
// GROUPS passed bit-fields
  class t {
      short           :(sizeof(short)-2);
    public:
      t();
  };
