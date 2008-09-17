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
  char* temp = (char *)obj;		// { dg-error "invalid cast" } 
  temp[0] = '\0';
}
