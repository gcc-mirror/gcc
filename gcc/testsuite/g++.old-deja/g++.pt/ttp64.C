// Build don't link:

// Origin: David Margery <David.Margery@irisa.fr>

// Bug: We fail to set DECL_TEMPLATE_PARM_P when reducing template
// parameter level.

template <class T> class A2 {
public:
   A2() {};
   virtual ~A2() {};
   template <class other> A2 & operator=(const A2<other> o) {
      i=o.i;
      return *this;
   };
   T i;
   T j;
};

template <class T> class A1 {
public:
   A1() {};
   virtual ~A1() {};
   template <class other> A1 & operator=(const A1<other> o) {
      i=o.i;
      return *this;
   };
   template <class other> A1 & operator=(const A2<other> o) {
      i=o.i;
      return *this;
   };
   T i;
};

template <template <class U> class T> class B {
public:
   B(){};
   virtual ~B(){};
   template <template <class U2> class O> struct rebind { typedef B<O> other ;};
   template <template <class U2> class O> B & operator=(const B<O> o) {
      i=o.i;
      return *this;
   };
   T<int> i;
};

int main(int argc, char *argv[]) {

   A1<int> a1;
   A1<long> a2;
   a1=a2;

   B<A1 > b1;
   B<A2 > b2;
   b1=b2;

   return 0;
}
