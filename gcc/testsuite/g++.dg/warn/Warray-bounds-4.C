// { dg-do compile }
// { dg-options "-O2 -Warray-bounds" }

class String
{
public:
  virtual unsigned long length() const = 0;
  virtual char get(unsigned long index) const = 0;
  virtual void set(unsigned long index, char value) = 0;
  virtual char& operator[] (unsigned long value) = 0;
  virtual ~String() {};
};

template<unsigned long size> class FixedString : public String
{
private:
  char contents[size];

public:
  virtual unsigned long length() const { return size; }
  virtual char get(unsigned long index) const { return contents[index]; }
  virtual void set(unsigned long index, char value) { contents[index] = value; }
  virtual char& operator[] (unsigned long index) { return contents[index]; }

  FixedString() { contents[0] = '\0'; } // { dg-warning "above array bounds" }
};

void print_length (const String& string);

int main()
{
  const FixedString<0> empty;

  print_length(empty);

  return 0;
}
