// Test that we resolve this case as mandated by the standard, but also
// warn about it.  We choose op char* not because it is a member of B --
// the standard says that all conversion ops are treated as coming from
// the type of the argument -- but because it is non-const.

struct A  {
  operator const char *() const;
};

struct B : public A {
  operator char *() { return 0; }
};

int main()
{
  B b;
  (const char *)b;		// WARNING - surprising overload resolution
}
