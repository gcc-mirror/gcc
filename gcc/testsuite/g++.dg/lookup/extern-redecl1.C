extern int X; // { dg-message "previous declaration" }
extern int Y (int);  // { dg-message "old declaration" }
extern int Y (float);

static int Z (int s) // { dg-message "old declaration" }
{
  return s;
}

void Foo ()
{
  extern char X; // { dg-error "conflicting declaration" }
  extern char Y (int); // { dg-error "ambiguating new declaration" }
  extern int Y (float);
  extern void Y (double);
  extern char Z (int); // { dg-error "ambiguating new declaration" }
}

