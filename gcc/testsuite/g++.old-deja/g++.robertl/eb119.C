template<bool B>
void f()
{
}

int main()
{
  f<bool>(); // ERROR - .*
}

