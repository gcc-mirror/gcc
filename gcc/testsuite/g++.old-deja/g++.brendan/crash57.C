// Build don't link: 
// GROUPS passed old-abort
class foo {
private:
  char buffer[1024];
public:
  foo();
};

main()
{
  static foo& a = *(new foo);
}
