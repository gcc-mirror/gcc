void f(const int &) { }
void f(const float &);

main()
{
    f(false);			// gets bogus error
}
