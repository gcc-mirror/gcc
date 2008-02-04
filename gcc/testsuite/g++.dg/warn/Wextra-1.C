// { dg-options "-Wextra" }

struct T {
  // If the implicitly-declared default constructor for "T" is
  // required, an error will be issued because "i" cannot be
  // initialized.  And, this class is not an aggregate, so it cannot
  // be brace-initialized.  Thus, there is no way to create an object
  // of this class.  We issue a warning with -Wextra.
  const int i;			// { dg-warning "const" }
private:
  int j;
};
