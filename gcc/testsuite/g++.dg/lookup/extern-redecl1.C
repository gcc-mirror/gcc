extern int X; // { dg-message "previous declaration" }
extern int Y (int);  // { dg-message "previous declaration" }
extern int Y (float);

static int Z (int s)
{
  return s;
}

void Foo ()
{
  extern char X; // { dg-error "local external declaration" }
  extern char Y (int); // { dg-error "local external declaration" }
  extern int Y (float);
  extern void Y (double);
  extern char Z (int);
}

