// { dg-options "-Wzero-as-null-pointer-constant" }

struct A;

typedef int (A::*pointmemfun) (int);
typedef int (A::*pointdmem);
typedef int (*pointfun) (int);

pointmemfun pmfs;
pointdmem   pdms;
pointfun    pfs;     
int*        ps;

void f()
{
  pointmemfun pmf(0);   // { dg-warning "zero as null pointer" }
  pointdmem   pdm(0);   // { dg-warning "zero as null pointer" }
  pointfun    pf(0);    // { dg-warning "zero as null pointer" }
  int*        p(0);     // { dg-warning "zero as null pointer" }

  pmf = 0;              // { dg-warning "zero as null pointer" }

  pdm = 0;              // { dg-warning "zero as null pointer" }

  pf = 0;               // { dg-warning "zero as null pointer" }

  p = 0;                // { dg-warning "zero as null pointer" }

  if (pmf)
    ;
  
  if (pdm)
    ;

  if (pf)
    ;

  if (p)
    ;

  if (!pmf)
    ;
  
  if (!pdm)
    ;

  if (!pf)
    ;

  if (!p)
    ;

  if (pmf == 0)         // { dg-warning "zero as null pointer" }
    ;
  
  if (pdm == 0)         // { dg-warning "zero as null pointer" }
    ;

  if (pf == 0)          // { dg-warning "zero as null pointer" }
    ;

  if (p == 0)           // { dg-warning "zero as null pointer" }
    ;

  if (0 == pmf)         // { dg-warning "zero as null pointer" }
    ;
  
  if (0 == pdm)         // { dg-warning "zero as null pointer" }
    ;

  if (0 == pf)          // { dg-warning "zero as null pointer" }
    ;

  if (0 == p)           // { dg-warning "zero as null pointer" }
    ;

  if (pmf != 0)         // { dg-warning "zero as null pointer" }
    ;
  
  if (pdm != 0)         // { dg-warning "zero as null pointer" }
    ;

  if (pf != 0)          // { dg-warning "zero as null pointer" }
    ;

  if (p != 0)           // { dg-warning "zero as null pointer" }
    ;

  if (0 != pmf)         // { dg-warning "zero as null pointer" }
    ;
  
  if (0 != pdm)         // { dg-warning "zero as null pointer" }
    ;

  if (0 != pf)          // { dg-warning "zero as null pointer" }
    ;

  if (0 != p)           // { dg-warning "zero as null pointer" }
    ;
}
