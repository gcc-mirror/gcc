extern void* malloc (__SIZE_TYPE__);

const char* write_strchr_literal (int x)
{
  char *p = __builtin_strchr ("123", x); 
  *p = 0; /* { dg-warning "dereference of NULL 'p'" "null deref" } */
  /* { dg-warning "write to string literal" "string literal" { target *-*-* } .-1 } */  
  return p;
}

const char* write_strchr_const_array (int x)
{
  static const char a[] = "123";
  char *p = __builtin_strchr (a, x);
  *p = 0; /* { dg-warning "dereference of NULL 'p'" "null deref" } */
  /* { dg-warning "write to 'const' object 'a'" "write to const" { target *-*-* } .-1 } */  
  return a;
}

char* write_function (void)
{
  char *p = (char*)malloc /* forgot arguments */;
  p[1] = 'a'; /* { dg-warning "write to function 'malloc'" } */
  __builtin_strcpy (p, "123");  /* { dg-warning "write to function 'malloc'" } */
  return p;
}

char* write_label (void)
{
  char *p = (char*)&&L;
  *p = 0; /* { dg-warning "write to label 'L'" } */
L:
  return p;
}

struct A { const int i; };

extern /* not const */ struct A a;

void write_const_member (void)
{
  char *p = (char*)&a.i;
  *p = 0;   // missing warning
}
