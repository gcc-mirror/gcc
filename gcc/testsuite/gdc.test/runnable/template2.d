// original post to the D newsgroup:
//    http://www.digitalmars.com/pnews/read.php?server=news.digitalmars.com&group=D&artnum=10554&header
// Test to manipulate 3D vectors, in D!
// by Sean L Palmer (seanpalmer@directvinternet.com)
// This code is released without any warranty.  Use at your own risk.
import core.stdc.stdio;
import std.math : sqrt;

template VecTemplate(tfloat, int dim:3)
{
 struct Vector
 {
  tfloat d[dim];

  version(none)
  {
   // sets the vector to the value of the given array
   void set(tfloat[dim] r) { d[] = r[]; }
   // comparison (a == b, a != b)
   bool opEquals(Vector b) { for (int i=0; i<dim; ++i) if (d[i] != b.d[i]) return
false; return true; }
   // negate (-a)
   Vector opNeg() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = -d[i]; return
r;  }
   // complement (~a)
   Vector opCom() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[(i+1)%dim];
d[0] = -d[0]; return r;  }
   // add (a + b)
   Vector opAdd(Vector b) { Vector r; r.d[] = d[] + b.d[]; return r;  }
   // addto (a += b)
   Vector opAddAssign(Vector b) { d[] += b.d[]; return r;  }
   // subtract (a - b)
   Vector opSub(Vector b) { Vector r; r.d[] = d[] - b.d[]; return r;  }
   // multiply by scalar (a * 2.0)
   Vector opMul(tfloat b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i]
* b; return r;  }
   // divide by scalar (a / b)
   Vector opDiv(tfloat b) { return *this * (1/b);  }
   // dot product (a * b)
   tfloat opMul(Vector b) { tfloat r=0; for (int i=0; i<dim; ++i) r += d[i];
return r;  }
   // outer product (a ^ b)
   //Vector opXor(Vector b) { Vector r; for (int i=0; i<dim; ++i) r += d[i]; return r;  }
  }

  void set(tfloat[dim] r) { for (int i=0; i<dim; ++i) d[i] = r[i]; }
  // comparison (a == b, a != b)
  const bool opEquals(ref const Vector b) { for (int i=0; i<dim; ++i) if (d[i] != b.d[i]) return
false; return true; }
  // negate (-a)
  Vector opNeg() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = -d[i]; return
r;  }
  // complement (~a)
  Vector opCom() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[(i+1)%dim];
d[0] = -d[0]; return r;  }
  // add (a + b)
  Vector opAdd(Vector b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i] +
b.d[i]; return r;  }
  // addto (a += b)
  Vector opAddAssign(Vector b) { for (int i=0; i<dim; ++i) d[i] += b.d[i]; return
this; }
  // subtract (a - b)
  Vector opSub(Vector b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i] -
b.d[i]; return r;  }
  // multiply by scalar (a * 2.0)
  Vector opMul(tfloat b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i] *
b; return r;  }
  // divide by scalar (a / b)
  Vector opDiv(tfloat b) { return this * (1/b);  }
  // dot product (a * b)
  tfloat opMul(Vector b) { tfloat r=0; for (int i=0; i<dim; ++i) r += d[i];
return r;  }
  // outer product (a ^ b)
  //Vector opXor(Vector b) { Vector r; for (int i=0; i<dim; ++i) r += d[i]; return r;  }

  void print() { for (int i=0; i<dim; ++i) printf("%f ", d[i]);
printf("\n"); }
 }

 tfloat abs(Vector v)
 {
  return sqrt(sqr(v));
 }

 tfloat sqr(Vector v)
 {
  return v * v;
 }
}

alias VecTemplate!(float, 3) VcT;

float[3] up = [ 0, 1, 0 ];

int main(char[][] args)
{
 printf("running\n");
 with (VcT)         // try this, I dare ya!!  crashes DMD 0.51
 {
  VcT.Vector a,b,c;
  c.set(up);
  b.set(up);
  a = b + c;
  a.print();
  printf("b * c = %f\n",b * c);
  printf("abs(a) = %f\n",VcT.abs(a));
  printf("sqr(a) = %f\n",VcT.sqr(a));
 }
 printf("closing\n");
 return 0;
}



