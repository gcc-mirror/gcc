/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
typedef struct s { const int i; } s; /* { dg-message "should be initialized" } */
union u {const int a; double b;}; /* { dg-message "should be initialized" } */
struct ts { int a; s v;};
struct ta { int a; s v[2];};

void f ()
{
  s v1; /* { dg-warning "uninitialized const member in" } */
  s va[2]; /* { dg-warning "uninitialized const member in" } */
  union u v2; /* { dg-warning "uninitialized const member in" } */ 
  struct ts v3; /* { dg-warning "uninitialized const member in" } */
  struct ta ta[2]; /* { dg-warning "uninitialized const member in" } */
} 
