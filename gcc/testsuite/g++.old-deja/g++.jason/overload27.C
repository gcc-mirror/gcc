void f(const int &) { }
void f(const float &);

int main()
{
    f(false);			// gets bogus error
}
