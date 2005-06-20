/* { dg-do compile } */

/* Test generic operations on vectors.  */

int __attribute__((vector_size(16))) a, b, c;
int __attribute__((vector_size(8))) d;
void foo()
{
     a = b ^ c;
     a = b + c;
     a = b - c;
     a = b * c;
     a = b / c;
     a = -b;
     a = d + b;		/* { dg-error "invalid operands to binary +" } */
} 
