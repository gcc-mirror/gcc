struct font {
  struct {
    char *line,*ulmask;
  } c[2];
} character[1] = { { {"", ""}, {"", ""} } }; /* { dg-error "extra|near|excess" } */
