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
    long i; // { dg-error "C language linkage" }
    double f; // { dg-error "C language linkage" }
    int fn (); // { dg-error "C language linkage" }
    int ai1[2]; // { dg-error "C language linkage" }
  }
}

