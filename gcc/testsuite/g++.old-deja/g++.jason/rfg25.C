// { dg-do assemble  }
struct { int :0; };  /* { dg-error "" } anon struct not used to declare objects */
