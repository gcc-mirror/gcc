// original post to the D newsgroup:
//    https://www.digitalmars.com/d/archives/10510.html#N10554
// Test to manipulate 3D vectors, in D!
// by Sean L Palmer (seanpalmer@directvinternet.com)
// This code is released without any warranty.  Use at your own risk.
import core.stdc.stdio;
import core.math : sqrt;

template VecTemplate(tfloat, int dim:3)
{
 struct Vector
 {
  tfloat[dim] d;

  version(none)
  {
   // sets the vector to the value of the given array
   void set(tfloat[dim] r) { d[] = r[]; }
   // comparison (a == b, a != b)
   bool opEquals(Vector b) { for (int i=0; i<dim; ++i) if (d[i] != b.d[i]) return
false; return true; }
   // negate (-a)
   Vector opUnary(string op : "-")() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = -d[i]; return
r;  }
   // complement (~a)
   Vector opUnary(string op : "~")() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[(i+1)%dim];
d[0] = -d[0]; return r;  }
   // add (a + b)
   Vector opBinary(string op : "+")(Vector b) { Vector r; r.d[] = d[] + b.d[]; return r;  }
   // addto (a += b)
   Vector opOpAssign(string op : "+")(Vector b) { d[] += b.d[]; return r;  }
   // subtract (a - b)
   Vector opBinary(string op : "-")(Vector b) { Vector r; r.d[] = d[] - b.d[]; return r;  }
   // multiply by scalar (a * 2.0)
   Vector opBinary(string op : "*")(tfloat b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i]
* b; return r;  }
   // divide by scalar (a / b)
   Vector opBinary(string op : "/")(tfloat b) { return *this * (1/b);  }
   // dot product (a * b)
   tfloat opBinary(string op : "*")(Vector b) { tfloat r=0; for (int i=0; i<dim; ++i) r += d[i];
return r;  }
   // outer product (a ^ b)
   //Vector opXor(Vector b) { Vector r; for (int i=0; i<dim; ++i) r += d[i]; return r;  }
  }

  void set(tfloat[dim] r) { for (int i=0; i<dim; ++i) d[i] = r[i]; }
  // comparison (a == b, a != b)
  const bool opEquals(ref const Vector b) { for (int i=0; i<dim; ++i) if (d[i] != b.d[i]) return
false; return true; }
  // negate (-a)
  Vector opUnary(string op : "-")() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = -d[i]; return
r;  }
  // complement (~a)
  Vector opUnary(string op : "~")() { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[(i+1)%dim];
d[0] = -d[0]; return r;  }
  // add (a + b)
  Vector opBinary(string op : "+")(Vector b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i] +
b.d[i]; return r;  }
  // addto (a += b)
  Vector opOpAssign(string op : "+")(Vector b) { for (int i=0; i<dim; ++i) d[i] += b.d[i]; return
this; }
  // subtract (a - b)
  Vector opBinary(string op : "-")(Vector b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i] -
b.d[i]; return r;  }
  // multiply by scalar (a * 2.0)
  Vector opBinary(string op : "*")(tfloat b) { Vector r; for (int i=0; i<dim; ++i) r.d[i] = d[i] *
b; return r;  }
  // divide by scalar (a / b)
  Vector opBinary(string op : "/")(tfloat b) { return this * (1/b);  }
  // dot product (a * b)
  tfloat opBinary(string op : "*")(Vector b) { tfloat r=0; for (int i=0; i<dim; ++i) r += d[i];
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
