double f(double);
float f(float);
void h(typeof(f) g) {} // { dg-error "" }
 
