// { dg-do assemble  }
// { dg-options "-Wcast-qual" }
// prms-id: 2855

class Ctest {
private:
  char* data;
public:
  operator const char *() const;
};

Ctest::operator const char *() const
{
  return data;
}
int main()
{
  Ctest obj;
  char* temp = reinterpret_cast<char *>(obj); // { dg-error "16:invalid cast" }
  temp[0] = '\0';
  char* temp2 = (char *)obj; // OK, const_cast<char *>(static_cast<char const *>(...))
}
