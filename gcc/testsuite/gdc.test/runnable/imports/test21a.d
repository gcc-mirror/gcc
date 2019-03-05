module imports.test21a;

struct TC(T)
{
 void method()
 {
     void inline_function()
     {
     }
 }
}

template TB(T)
{
 alias TC!(T) tc_instance;
}

struct TA(T)
{
 mixin TB!(T);
}
