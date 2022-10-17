module issue15818;

void f(int);
void f(int);
void f(int);
void f(int);
void f(int);

pragma(mangle, "_D10issue158181fFiZv")
void theVeritableF(int){}

void main()
{
    f(1);
}
