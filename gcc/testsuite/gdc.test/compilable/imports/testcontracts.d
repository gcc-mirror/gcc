module imports.testcontracts;

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=3602

class Base3602
{
   void method(int x, int y)
   in
   {
       assert(x > 0);
       assert(y > 0);
   }
   do
   {
   }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=5230

class Base5230
{
    int method()
    out (res)
    {
    }
    do
    {
        return 42;
    }
}
