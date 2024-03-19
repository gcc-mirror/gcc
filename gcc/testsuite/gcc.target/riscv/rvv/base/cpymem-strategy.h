typedef struct { unsigned char a[56]; } a56;
typedef struct { int b[32]; } b32;

void f1 (a56 *v1, a56 *v2)
{
    *v1 = *v2;
}

void f2 (b32 *v1, b32 *v2)
{
    *v1 = *v2;
}
