// { dg-do run  }
void f(const int &) { }
void f(const float &);

int main()
{
    f(false);			// { dg-bogus "" } 
}
