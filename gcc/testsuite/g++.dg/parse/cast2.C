// PR c++/13736

struct string
{
  string() {}
  string(const string&) {}
  string(const char*) {}
};

int main()
{
  string s2(string( (const char*)("")));
}
