// Build don't link: 
// GROUPS passed bit-fields
  class t {
      short           :(sizeof(short)-2);
    public:
      t();
  };
