// Build don't link: 
// GROUPS passed bit-fields
struct bar {
  int : 2 = 1;// ERROR - .*
};
