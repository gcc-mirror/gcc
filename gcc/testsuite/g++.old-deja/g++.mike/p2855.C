// Build don't link:
// Special g++ Options: -Wcast-qual
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
  char* temp = (char *)obj;		// WARNING - 
  temp[0] = '\0';
}
