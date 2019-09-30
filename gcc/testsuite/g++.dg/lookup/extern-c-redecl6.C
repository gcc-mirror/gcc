extern "C" {
  int i; // { dg-message "previous" }
  float f; // { dg-message "previous" }
  void fn (); // { dg-message "previous" }
  int ai1[1]; // { dg-message "previous" }
  extern int ai[];

  namespace OK
  {
    int i;
    float f;
    void fn ();
    extern int ai1[];
    int ai[2];
  }

  namespace BAD
  {
    long i; // { dg-error "10:conflicting C language linkage" }
    double f; // { dg-error "12:conflicting C language linkage" }
    int fn (); // { dg-error "9:conflicting C language linkage" }
    int ai1[2]; // { dg-error "9:conflicting C language linkage" }
  }
}

