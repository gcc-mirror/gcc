// PRMS Id: 5124
// Bug: g++ promotes bar to int* too soon and the call to f fails.
// Build don't link:

typedef int arr[1];

struct A {
   void f(void);
   void f(arr &);

   void g(void);
   void g(int *);

   void h(void);
};


void A::h(void)
{
   arr bar;
   f(bar);
   g(bar);
}
