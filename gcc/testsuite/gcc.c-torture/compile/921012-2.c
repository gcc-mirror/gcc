struct foo {
int a,b,c;
};
void
f(struct foo*a,struct foo*b)
{
*a=*b;
}
