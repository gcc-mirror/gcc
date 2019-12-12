class a { // { dg-lto-message "a different type is defined in another translation unit" }
  int *b() const;
};
int *a::b() const { return 0; }
