/* { dg-do compile } */

// From PR28573

struct A
{
   int operator [] ( int indx ) { return indx; }
};

struct B
{
   A a;
};

int main()
{
   return __builtin_offsetof(B, a[0]);  /* { dg-error "cannot apply.*offsetof" } */
}
