// PR c++/31411

struct allocator{
  ~allocator() throw();
};
struct string
{
  string(const string& str, const allocator& al = allocator());
};
int main() {
  try {}
  catch (string smess) {}
}
