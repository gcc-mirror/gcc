namespace std {
class Base {};
}

struct Derived : public std::Base {
  operator const char*() const;
  operator bool(void) const;
};

void log(const char* str);

void nothing()
{
  Derived temp;
  log(temp);
}
