// Build don't link:
// Origin: mrs@wrs.com (Mike Stump)

class Wrapper {
public:
  static const char msgPtr[];
  static const char *JunkFunc() {
    return &msgPtr[0];
  }
};
 
const char Wrapper::msgPtr[] = "Hello world.";
 
int main() {
  const char *p1 = &Wrapper::msgPtr[0];
  const char *p2 = Wrapper::JunkFunc();
 
  if (p1 != p2)
    return 1;
}
