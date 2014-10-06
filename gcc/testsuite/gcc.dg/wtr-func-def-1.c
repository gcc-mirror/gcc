/* Test for -Wtraditional warnings on ISO C function definitions.
   Note, gcc should omit these warnings in system header files.
   Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 6/30/2002.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional -std=gnu89" } */

/* Test some simple cases.  */

void f_void1 (void) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

void f_void2 ()
{
  return;
}

void f_int1 (int f) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

void f_int2 (f)
     int f;
{
  return;
}

/* Test that we don't ever warn about nested functions.  */

void f_int3 (int f) /* { dg-warning "traditional C rejects ISO C style" } */
{
  void f3a (void) { return; }
  void f3b () { return; }
  void f3c (int f) { return; }
  void f3d (f) int f; { return; }
  void f3e (const char *f, ...) { return; }
  return;
}

void f_int4 (int f) /* { dg-warning "traditional C rejects ISO C style" } */
{
  void f4a (void) { return; }
  void f4b () { return; }
  void f4c (int f) { return; }
  void f4d (f) int f; { return; }
  void f4e (const char *f, ...) { return; }
  auto f4f (void) { return 0; }
  return;
}

void f_int5 (f)
     int f;
{
  void f5a (void) { return; }
  void f5b () { return; }
  void f5c (int f) { return; }
  void f5d (f) int f; { return; }
  void f5e (const char *f, ...) { return; }
  return;
}

void f_int6 (f)
     int f;
{
  void f6a (void) { return; }
  void f6b () { return; }
  void f6c (int f) { return; }
  void f6d (f) int f; { return; }
  void f6e (const char *f, ...) { return; }
  auto f6f (void) { return 0; }
  return;
}

/* Test that prototypes are silently accepted and function definitions
   are still warned about.  */

extern void f_int_p1 (int);
void f_int_p1 (int f) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

extern void f_int_p2 (int f);
void f_int_p2 (int f) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

extern void f_int_p3 (int);
void f_int_p3 (f)
     int f;
{
  return;
}

extern void f_int_p4 (int f);
void f_int_p4 (f)
     int f;
{
  return;
}

extern void f_void_p1 ();
void f_void_p1 (void) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

extern void f_void_p2 (void);
void f_void_p2 (void) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

extern void f_blank_p1 ();
void f_blank_p1 ()
{
  return;
}

extern void f_blank_p2 (void);
void f_blank_p2 ()
{
  return;
}

/* Test some implicit int functions.  */

f_impl1()
{
  return 0;
}

f_impl2(void) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return 0;
}

f_impl3(int f) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return 0;
}

/* Test stdarg functions.  */

f_stdarg1(const char *s, ...) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return 0;
}

void f_stdarg2(const char *s, ...) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

extern void f_stdarg3(const char *, ...);
void f_stdarg3(const char *s, ...) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

/* Test handling function pointer parameters.  */

void f_fnptr1 (int f, int (*fp)(int));
void f_fnptr1 (int f, int (*fp)(int)) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return;
}

void f_fnptr2 (int f, int (*fp)(int));
void f_fnptr2 (f, fp)
     int f;
     int (*fp)(int);
{
  return;
}

/* Test for main.  */

int
main (int argc, char **argv) /* { dg-warning "traditional C rejects ISO C style" } */
{
  return 0;
}

# 182 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

void fsys1 (void)
{
  return;
}

void fsys2 (int f)
{
  return;
}

void fsys3 (const char *f, ...)
{
  return;
}
