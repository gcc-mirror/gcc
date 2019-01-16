struct S
{
   template opDispatch (string name) {}
}
void main()
{
    S s;
    s.x!int;
}
