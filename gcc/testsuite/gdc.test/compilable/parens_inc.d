// Test UnaryExp (expr)++ parsing

void main(){
    int[2] y;
    int *x = y.ptr;
    *(x)++=0;
    (*(x)--)=0;
    (*x++)=0; // ok
    int a = 1, b = 2;
    int*[] z = [&a, &b];
    *(z[0])++=0; //ok
    (y[0])--;
    *x++=0;
}

void f()
{
    int b;
    (b)++;
    int[] a = [1];
    b = (a)[0]++; //ok
    (a[0])--;
    b = (int).init; //ok
}
