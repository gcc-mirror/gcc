
namespace std{ 
  int f(){
    return 0;
  }
}

int f()
{
  return 1;
}

int main()
{
  return std::f();
}
