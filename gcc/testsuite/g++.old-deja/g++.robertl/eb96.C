class foo
{
  public:
   static int f();

   class bar {
     friend int foo::f();
   };
};

int main()
{
   return 0;
}
